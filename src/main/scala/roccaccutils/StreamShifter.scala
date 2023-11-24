// See LICENSE for license details

package roccaccutils

import chisel3._
import chisel3.util._

import org.chipsalliance.cde.config.{Parameters, Config}
import freechips.rocketchip.unittest.{UnitTest, UnitTests}
import freechips.rocketchip.system.{BaseConfig}
import icenet.{StreamChannel}

/**
 * Modified ring buffer with the following properties:
 *   - Shifts a StreamChannel's worth of data based on 'read_bytes' output
 *   - Can only shift one stream at a time (i.e. must wait until all bytes in a stream are read or
 *     'done_reading' is set and clears it)
 *   - Can only enqueue into buffer when there is 'maxInWidthBits' avail. in the underlying buffer
 *     (must be able to put the entire stream into the buffer)
 */
// TODO: add 'done_reading' to clear buffer and start new stream
class StreamShifter(maxInWidthBits: Int, capacityBytes: Int) extends Module {
  val w = maxInWidthBits
  val wB = maxInWidthBits / 8
  val wL2Up = log2Up(maxInWidthBits) + 1

  require(wB <= capacityBytes) // recommended that capacityBytes >>> wB

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

  // storage elements
  val mem = RegInit(VecInit(Seq.fill(capacityBytes)(0.U(8.W))))
  val head = RegInit(0.U((log2Ceil(capacityBytes)).W))
  val tail = RegInit(0.U((log2Ceil(capacityBytes)).W))
  val wrapBitHead = RegInit(false.B)
  val wrapBitTail = RegInit(false.B)
  val done_enq = RegInit(false.B)

  // wrap signal
  val wrap = (capacityBytes - 1).U

  // full and empty conditions
  val full = WireDefault(false.B)
  val empty = WireDefault(false.B)

  // count related vars
  val count = WireDefault(0.U((log2Ceil(capacityBytes) + 1).W))
  when (wrapBitHead === wrapBitTail) {
    count := head - tail
  } .otherwise {
    count := (capacityBytes.U - tail) +& head
  }
  val space_avail = capacityBytes.U - count
  val one_beat_finish = (count <= wB.U)
  // must be able to enq. max bytes
  val can_enq = space_avail >= wB.U
  //printf("Count:%d SpaceAvail:%d OBF:%d CanEnq:%d Empty:%d Full:%d\n",
  //  count, space_avail, one_beat_finish, can_enq, empty, full)
  //printf("Head:%d Tail:%d\n", head, tail)

  when (head === tail) {
    when (wrapBitHead === wrapBitTail) {
      empty := true.B
    }.otherwise {
      full := true.B
    }
  }

  def updatePtrAndWrapTracker(ptr: UInt, add: UInt, wrap_tracker: Bool): (UInt, Bool) = {
    ((ptr +& add) % capacityBytes.U, Mux(ptr +& add >= capacityBytes.U, !wrap_tracker, wrap_tracker))
  }
  def updatePtr(ptr: UInt, add: UInt): UInt = {
    val (new_ptr, _) = updatePtrAndWrapTracker(ptr, add, false.B) // tracker unused
    new_ptr
  }

  // write operation
  when (io.in.fire) {
    for (i <- 0 until wB) {
      when (io.in.bits.keep(i) === 1.U) {
        mem(updatePtr(head, i.U)) := io.in.bits.data((8*(i+1))-1, 8*i)
      }
    }

    //printf("Updating mem:\n")
    //for (i <- 0 until wB) {
    //  when (io.in.bits.keep(i) === 1.U) {
    //    printf("mem[%d] : %x\n",
    //      updatePtr(head, i.U),
    //      io.in.bits.data((8*(i+1))-1, 8*i))
    //  }
    //}

    val (hWire, wbhWire) = updatePtrAndWrapTracker(head, PopCount(io.in.bits.keep), wrapBitHead)
    dontTouch(wbhWire)
    head := hWire
    wrapBitHead := wbhWire
  }

  done_enq := Mux(io.in.fire, io.in.bits.last, Mux(empty, false.B, done_enq))

  // read operation
  val out_data = Wire(Vec(wB, UInt(8.W)))
  for (i <- 0 until wB) {
    out_data(i) := mem(updatePtr(tail, i.U))
  }
  io.out.bits.data := Cat((0 until wB).map(out_data(_)).reverse)
  io.out.bits.last := done_enq && one_beat_finish
  io.out.bits.bytes_avail := Mux(one_beat_finish, count, wB.U)

  when (io.out.fire()) {
    //printf("Reading mem:\n")
    //for (i <- 0 until wB) {
    //  printf("%x : mem[%d]\n",
    //    mem(updatePtr(tail, i.U)),
    //    updatePtr(tail, i.U))
    //}

    val (tWire, wbtWire) = updatePtrAndWrapTracker(tail, io.out.bits.read_bytes, wrapBitTail)
    tail := tWire
    wrapBitTail := wbtWire
  }

  io.in.ready := !full && !done_enq && can_enq
  io.out.valid := !empty

  when (io.in.fire) {
    printf("IN: Last:%d Keep:%x Data:%x\n",
      io.in.bits.last,
      io.in.bits.keep,
      io.in.bits.data)
  }

  when (io.out.fire()) {
    printf("OUT: Last:%d BA:%d RB:%d Data:%x\n",
      io.out.bits.last,
      io.out.bits.bytes_avail,
      io.out.bits.read_bytes,
      io.out.bits.data)
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

  val ss = Module(new StreamShifter(streamBits, 2 * (streamBits / 8)))

  val (inIdx, inDone) = Counter(ss.io.in.fire, inData.size)
  val (outIdx, outDone) = Counter(ss.io.out.fire(), outData.size)
  printf("inIdx:%d, outIdx: %d outBytesAvail:%d\n", inIdx, outIdx, outBytesAvail(outIdx))

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

  def compareData(a: UInt, b: UInt, bytes_read: UInt) = {
    val bitmask = (1.U << (bytes_read * 8.U)) - 1.U
    (a & bitmask) === (b & bitmask)
  }

  val compValues = Seq(
    ("data", compareData(ss.io.out.bits.data, outData(outIdx), ss.io.out.bits.read_bytes)),
    ("bytes_avail", ss.io.out.bits.bytes_avail === outBytesAvail(outIdx)),
    ("last", ss.io.out.bits.last === outLast(outIdx)),
  )
  when (ss.io.out.fire()) {
    printf("TEST OUT: Last:%d BA:%d RB:%d Data:%x\n",
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
