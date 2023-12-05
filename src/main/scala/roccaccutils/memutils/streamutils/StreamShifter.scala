// See LICENSE for license details

package roccaccutils.memutils

import chisel3._
import chisel3.util._

import org.chipsalliance.cde.config.{Parameters, Config}
import freechips.rocketchip.unittest.{UnitTest, UnitTests}
import freechips.rocketchip.system.{BaseConfig}
import freechips.rocketchip.util.{DecoupledHelper}
import midas.targetutils.{FireSimQueueHelper}
import roccaccutils.logger._

class StreamShifter(maxInWidthBits: Int, queueDepths: Int) extends Module {
  val w = maxInWidthBits
  val wB = maxInWidthBits / 8
  val wL2Up = log2Up(maxInWidthBits) + 1

  val io = IO(new Bundle {
    val in = Flipped(Decoupled(new StreamChannel(w)))
    val out = new Bundle {
      val ready = Input(Bool())
      val valid = Output(Bool())
      val bits = new Bundle {
        val data = Output(UInt(w.W))
        val last = Output(Bool())
        val bytes_avail = Output(UInt(wL2Up.W))
        // also read on fire
        val read_bytes = Input(UInt(wL2Up.W)) // combinationally determined by this bytes_avail output
      }

      def fire(): Bool = ready && valid
    }
  })

  when (io.in.fire) {
    assert(PopCount(io.in.bits.keep + 1.U) <= 1.U, "in.keep bits must be aligned (i.e. like 0xFF)")
  }

  LogUtils.logHexItems(
    io.in.fire,
    Seq(
      ("last", io.in.bits.last),
      ("keep", io.in.bits.keep),
      ("data", io.in.bits.data),
    ),
    Some(":SS:IN:"),
    oneline=true
  )

  LogUtils.logHexItems(
    io.out.fire(),
    Seq(
      ("last", io.out.bits.last),
      ("bytes_avail", io.out.bits.bytes_avail),
      ("read_bytes", io.out.bits.read_bytes),
      ("data", io.out.bits.data),
    ),
    Some(":SS:OUT:"),
    oneline=true
  )

  val write_start_index = RegInit(0.U(wL2Up.W))

  class ByteBundle extends Bundle {
    val byte = UInt(8.W)
    val last = Bool()
  }
  val shift_queues_enq = Wire(Vec(wB, new DecoupledIO(new ByteBundle)))
  val shift_queues_deq = VecInit((0 until wB).map(i => FireSimQueueHelper.makeDeqIO(shift_queues_enq(i), queueDepths, isFireSim=true)))
  // override further down
  for (i <- 0 until wB) {
    shift_queues_enq(i).valid := false.B
    shift_queues_enq(i).bits := DontCare
    shift_queues_deq(i).ready := false.B
  }

  val len_to_write = PopCount(io.in.bits.keep)

  for (i <- 0 until wB) {
    val remapindex = (write_start_index +& i.U) % wB.U
    shift_queues_enq(remapindex).bits.byte := io.in.bits.data >> (i * 8)
    shift_queues_enq(remapindex).bits.last := io.in.bits.last && ((i.U +& 1.U) === len_to_write) // only mark last byte
  }

  val idata = Seq.fill(wB)("(%d:%d:0x%x,%d)").mkString(" ")
  val idataVals = (0 until wB).map(i => Seq(i.U, shift_queues_enq(i).valid, shift_queues_enq(i).bits.byte, shift_queues_enq(i).bits.last)).flatten
  DefaultLogger.logInfo("queue.enq: " + idata + "\n", idataVals:_*)

  val wrap_len_index_wide = (write_start_index +& len_to_write)
  val wrap_len_index_end = (wrap_len_index_wide % wB.U)
  val wrapped = (wrap_len_index_wide >= wB.U)

  val all_queues_ready = shift_queues_enq.map(_.ready).reduce(_ && _)

  io.in.ready := all_queues_ready
  when (io.in.valid && all_queues_ready) {
    write_start_index := wrap_len_index_end
  }

  for (i <- 0 until wB) {
    val i_gte_start = (i.U >= write_start_index)
    val i_lt_end = (i.U < wrap_len_index_end)
    val use_this_queue = Mux(wrapped, i_gte_start || i_lt_end, i_gte_start && i_lt_end)
    shift_queues_enq(i).valid := use_this_queue && io.in.valid && all_queues_ready
  }

  // ----------------------------------

  val read_start_index = RegInit(0.U(wL2Up.W))

  val remapVecLast = Wire(Vec(wB, Bool()))
  val remapVecData = Wire(Vec(wB, UInt(8.W)))
  val remapVecValids = Wire(Vec(wB, Bool()))
  val remapVecReadys = Wire(Vec(wB, Bool()))

  for (i <- 0 until wB) {
    val remapindex = (i.U +& read_start_index) % wB.U
    remapVecData(i) := shift_queues_deq(remapindex).bits.byte
    remapVecLast(i) := shift_queues_deq(remapindex).bits.last
    remapVecValids(i) := shift_queues_deq(remapindex).valid
    shift_queues_deq(remapindex).ready := remapVecReadys(i)
  }

  val data = Seq.fill(wB)("(%d:%d:0x%x,%d)").mkString(" ")
  val dataVals = (0 until wB).map(i => Seq(i.U, shift_queues_deq(i).valid, shift_queues_deq(i).bits.byte, shift_queues_deq(i).bits.last)).flatten
  DefaultLogger.logInfo("queue.deq: " + data + "\n", dataVals:_*)

  val rdata = Seq.fill(wB)("(%d:%d:0x%x,%d)").mkString(" ")
  val rdataVals = (0 until wB).map(i => Seq(i.U, remapVecValids(i), remapVecData(i), remapVecLast(i))).flatten
  DefaultLogger.logInfo("remapVecs: " + rdata + "\n", rdataVals:_*)

  val valid_lasts = remapVecValids.zip(remapVecLast).map{case (v, l) => v && l}
  val has_last_byte = valid_lasts.reduce(_ || _)
  val count_valids_all = remapVecValids.map(_.asUInt).reduce(_ +& _)
  val count_valids_upper_bound_inc = PriorityEncoder(valid_lasts)
  val count_valids_bounded = remapVecValids.zipWithIndex.map{case (v, i) => Mux(i.U <= count_valids_upper_bound_inc, v, false.B)}.map(_.asUInt).reduce(_ +& _)
  val count_valids = Mux(has_last_byte,
    count_valids_bounded,
    count_valids_all
  )

  LogUtils.logHexItems(
    io.out.fire(),
    Seq(
      ("has_last_byte", has_last_byte),
      ("count_valids_all", count_valids_all),
      ("count_valids_upper_bound_inc", count_valids_upper_bound_inc),
      ("count_valids_bounded", count_valids_bounded),
    ),
    oneline=true,
  )

  io.out.bits.bytes_avail := count_valids
  io.out.bits.data := Cat(remapVecData.reverse)
  io.out.bits.last := has_last_byte

  val read_fire = DecoupledHelper(
    io.out.ready,
    remapVecValids.reduce(_ || _)
  )

  io.out.valid := read_fire.fire(io.out.ready)

  for (i <- 0 until wB) {
    remapVecReadys(i) := (i.U < io.out.bits.read_bytes) && read_fire.fire()
  }

  when (read_fire.fire()) {
    read_start_index := (read_start_index +& io.out.bits.read_bytes) % wB.U
  }
}

class ShiftStreamTest extends UnitTest {
  val streamBits = 8 * 8

  val inData = VecInit(
    "h0011223344556677".U,
    "h8899AABBCCDDEEFF".U,
    "h0123456789ABCDEF".U,
    "hFEDCBA9876543210".U)
  val inKeep = VecInit(
    "h3F".U,
    "hF".U,
    "hF".U,
    "hF".U)
  val inLast = VecInit(false.B, false.B, true.B, true.B)

  val outReadBytes = VecInit(
    2.U,
    2.U,
    4.U,
    6.U,
    2.U,
    2.U)
  val outBytesAvail = VecInit(
    6.U,
    8.U,
    8.U,
    6.U,
    4.U,
    2.U)
  val outLast = VecInit(
    0.U,
    0.U,
    0.U,
    1.U,
    1.U,
    1.U)
  val outData = VecInit(
    "h6677".U,
    "h4455".U,
    "hEEFF2233".U,
    "h89ABCDEFCCDD".U,
    "h3210".U,
    "h7654".U)

  val started = RegInit(false.B)
  val sending = RegInit(false.B)
  val receiving = RegInit(false.B)

  val ss = Module(new StreamShifter(streamBits, 10))

  val (inIdx, inDone) = Counter(ss.io.in.fire, inData.size)
  val (outIdx, outDone) = Counter(ss.io.out.fire(), outData.size)
  DefaultLogger.logInfo("inIdx:%d, outIdx: %d outBytesAvail:%d\n", inIdx, outIdx, outBytesAvail(outIdx))

  ss.io.in.valid := sending
  ss.io.in.bits.data := inData(inIdx)
  ss.io.in.bits.keep := inKeep(inIdx)
  ss.io.in.bits.last := inLast(inIdx)
  ss.io.out.ready := receiving
  ss.io.out.bits.read_bytes := outReadBytes(outIdx)

  when (io.start && !started) {
    started := true.B
    sending := true.B
    receiving := true.B
  }
  when (inDone) { sending := false.B }
  when (outDone) { receiving := false.B }

  io.finished := started && !sending && !receiving

  def compareData(a: UInt, b: UInt, read_bytes: UInt) = {
    val bitmask = (1.U << (read_bytes * 8.U)) - 1.U
    (a & bitmask) === (b & bitmask)
  }

  val compValues = Seq(
    ("data", compareData(ss.io.out.bits.data, outData(outIdx), ss.io.out.bits.read_bytes)),
    ("bytes_avail", ss.io.out.bits.bytes_avail === outBytesAvail(outIdx)),
    ("last", ss.io.out.bits.last === outLast(outIdx)),
  )
  when (ss.io.out.fire()) {
    DefaultLogger.logInfo("TEST OUT: Last:%d BA:%d RB:%d Data:%x\n",
      ss.io.out.bits.last,
      ss.io.out.bits.bytes_avail,
      ss.io.out.bits.read_bytes,
      ss.io.out.bits.data)
  }
  for (kv <- compValues) {
    assert(!ss.io.out.fire() || kv._2, s"'${kv._1}' not matching")
  }
}

class WithShiftStreamUnitTests extends Config((site, here, up) => {
  case UnitTests => (p: Parameters) => {
    Seq(
      Module(new ShiftStreamTest),
    )
  }
})

class ShiftStreamUnitTestConfig extends Config(
  new WithShiftStreamUnitTests ++ new BaseConfig)
