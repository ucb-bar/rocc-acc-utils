// See LICENSE for license details

package roccaccutils.memutils

import chisel3._
import chisel3.util._

import org.chipsalliance.cde.config.{Parameters}
import freechips.rocketchip.rocket.{TLBConfig}
import freechips.rocketchip.util.{DecoupledHelper}
import freechips.rocketchip.rocket.constants.{MemoryOpConstants}
import roccaccutils.logger._

class WriterBundle(implicit val hp: L2MemHelperParams) extends Bundle with HasL2MemHelperParams {
  val data = UInt(BUS_SZ_BITS.W)
  val validbytes = UInt((BUS_SZ_BYTES_LG2UP + 1).W)
  val end_of_message = Bool()
}

class DstInfo extends Bundle {
  val op = UInt(64.W) // output pointer for streaming mem data
  val cmpflag = UInt(64.W) // output pointer for completion flag
}

class MemWriter(val cmdQueueDepth: Int = 4, val writeCmpFlag: Boolean = true, val logger: Logger = DefaultLogger)(implicit p: Parameters, val hp: L2MemHelperParams)
  extends Module
  with MemoryOpConstants
  with HasL2MemHelperParams {

  val io = IO(new Bundle {
    val l2io = new L2MemHelperBundle

    val memwrites_in = Flipped(Decoupled(new WriterBundle))
    val dest_info = Flipped(Decoupled(new DstInfo))

    val bufs_completed = Output(UInt(64.W))
    val no_writes_inflight = Output(Bool())
  })

  val incoming_writes_queue = Module(new Queue(new WriterBundle, cmdQueueDepth))
  incoming_writes_queue.io.enq <> io.memwrites_in

  LogUtils.logHexItems(
    incoming_writes_queue.io.enq.fire(),
    Seq(
      ("data", incoming_writes_queue.io.enq.bits.data),
      ("validbytes", incoming_writes_queue.io.enq.bits.validbytes),
      ("EOM", incoming_writes_queue.io.enq.bits.end_of_message),
    ),
    Some("incoming_writes_queue.enq"),
    logger=logger)

  val dest_info_queue = Module(new Queue(new DstInfo, cmdQueueDepth))
  dest_info_queue.io.enq <> io.dest_info

  LogUtils.logHexItems(
    dest_info_queue.io.enq.fire(),
    Seq(
      ("op", dest_info_queue.io.enq.bits.op),
      ("cmpflag", dest_info_queue.io.enq.bits.cmpflag),
    ),
    Some("dest_info_queue.enq"),
    oneline=true,
    logger=logger)

  LogUtils.logHexItems(
    dest_info_queue.io.deq.fire(),
    Seq(
      ("op", dest_info_queue.io.deq.bits.op),
      ("cmpflag", dest_info_queue.io.deq.bits.cmpflag),
    ),
    Some("dest_info_queue.deq"),
    oneline=true,
    logger=logger)

  // read out data, shifting based on user input
  val shiftstream = Module(new StreamShifter(BUS_SZ_BITS, 2*BUS_SZ_BITS))
  shiftstream.io.in.valid := incoming_writes_queue.io.deq.valid
  incoming_writes_queue.io.deq.ready := shiftstream.io.in.ready
  shiftstream.io.in.bits.data := incoming_writes_queue.io.deq.bits.data
  shiftstream.io.in.bits.keep := BUS_BYTE_MASK >> (BUS_SZ_BYTES.U - incoming_writes_queue.io.deq.bits.validbytes)
  shiftstream.io.in.bits.last := incoming_writes_queue.io.deq.bits.end_of_message

  LogUtils.logHexItems(
    shiftstream.io.in.fire(),
    Seq(
      ("data", shiftstream.io.in.bits.data),
      ("keep", shiftstream.io.in.bits.keep),
      ("validbytes", incoming_writes_queue.io.deq.bits.validbytes),
      ("last", shiftstream.io.in.bits.last),
    ),
    Some("shiftstream.in"),
    logger=logger)

  val stream_bytes_avail = shiftstream.io.out.bits.bytes_avail

  val backend_bytes_written = RegInit(0.U(64.W))
  val backend_next_write_addr = dest_info_queue.io.deq.bits.op +& backend_bytes_written

  val busBytesMeta = (0 to BUS_SZ_BYTES_LG2UP).map(i => (i, scala.math.pow(2,i).toInt))

  // get the max number of bytes you can write on this xact aligned to pow2 (determined by address given)
  val max_bytes_writeable_aligned = MuxCase(BUS_SZ_BYTES.U, busBytesMeta.dropRight(1).map{case (i, j) => (backend_next_write_addr(i) -> j.U)})
  val max_bytes_writeable_aligned_log2 = MuxCase(BUS_SZ_BYTES_LG2UP.U, busBytesMeta.dropRight(1).map{case (i, j) => (backend_next_write_addr(i) -> i.U)})

  // get the max number of available bytes aligned to pow2
  val max_bytes_available_aligned = MuxCase(0.U, busBytesMeta.map{case (i, j) => (stream_bytes_avail(i) -> j.U)}.reverse)
  val max_bytes_available_aligned_log2 = MuxCase(0.U, busBytesMeta.map{case (i, j) => (stream_bytes_avail(i) -> i.U)}.reverse)

  val bytes_to_write = max_bytes_writeable_aligned.min(max_bytes_available_aligned)
  val bytes_to_write_log2 = max_bytes_writeable_aligned_log2.min(max_bytes_available_aligned_log2)
  val has_bytes_to_write = (bytes_to_write =/= 0.U)

  shiftstream.io.out.bits.read_bytes := bytes_to_write

  LogUtils.logHexItems(
    shiftstream.io.out.fire(),
    Seq(
      ("data", shiftstream.io.out.bits.data),
      ("last", shiftstream.io.out.bits.last),
      ("bytes_avail", shiftstream.io.out.bits.bytes_avail),
      ("read_bytes", shiftstream.io.out.bits.read_bytes),
    ),
    Some("shiftstream.out"),
    logger=logger)

  val end_of_stream = shiftstream.io.out.valid && shiftstream.io.out.bits.last && (stream_bytes_avail === bytes_to_write)
  assert(!end_of_stream || (end_of_stream && has_bytes_to_write), "Stream ends must have bytes to write")

  val s_write_stream :: s_write_cmpflag :: Nil = Enum(2)
  val state = RegInit(s_write_stream)
  switch (state) {
    is (s_write_stream) {
      when (io.l2io.req.fire() && end_of_stream) {
        state := s_write_cmpflag
      }
    }
    is (s_write_cmpflag) {
      when (io.l2io.req.fire()) {
        state := s_write_stream
      }
    }
  }
  val in_s_write_cmpflag = if (writeCmpFlag) (state === s_write_cmpflag) else false.B // could be true.B when !end_of_stream is true.B (this should take prio)
  val is_done_writing = if (writeCmpFlag) in_s_write_cmpflag else end_of_stream

  val mem_write_fire = DecoupledHelper(
    io.l2io.req.ready,
    dest_info_queue.io.deq.valid,
    shiftstream.io.out.valid,
  )

  io.l2io.req.valid := mem_write_fire.fire(io.l2io.req.ready) && (has_bytes_to_write || in_s_write_cmpflag)
  io.l2io.req.bits.size := Mux(in_s_write_cmpflag,                                 0.U,          bytes_to_write_log2)
  io.l2io.req.bits.addr := Mux(in_s_write_cmpflag, dest_info_queue.io.deq.bits.cmpflag,      backend_next_write_addr)
  io.l2io.req.bits.data := Mux(in_s_write_cmpflag,                                 1.U, shiftstream.io.out.bits.data)
  io.l2io.req.bits.cmd := M_XWR
  io.l2io.resp.ready := true.B // sync any write resp

  LogUtils.logHexItems(
    io.l2io.req.fire(),
    Seq(
      ("EOS", end_of_stream),
      ("has_bytes_to_write", has_bytes_to_write),
    ),
    Some("l2_fire"),
    oneline=true,
    logger=logger
  )

  val streams_completed = RegInit(0.U(64.W))

  when (io.l2io.req.fire()) {
    val next_backend_bytes_written = Mux(in_s_write_cmpflag, 0.U, Mux(end_of_stream, 0.U, backend_bytes_written +& bytes_to_write))
    val next_streams_completed = streams_completed +& is_done_writing.asUInt
    backend_bytes_written := next_backend_bytes_written
    streams_completed := next_streams_completed

    LogUtils.logHexItems(
      true.B,
      Seq(
        ("next_backend_bytes_written", next_backend_bytes_written),
        ("next_streams_completed", next_streams_completed),
        ("backend_bytes_written", backend_bytes_written),
        ("streams_completed", streams_completed),
      ),
      Some("update_ctrs"),
      oneline=true,
      logger=logger
    )
  }

  dest_info_queue.io.deq.ready := mem_write_fire.fire(dest_info_queue.io.deq.valid) && is_done_writing
  // TODO: any comb loop here (io.out.valid -> comb -> io.out.ready -> ss comb -> io.out.valid)?
  shiftstream.io.out.ready := mem_write_fire.fire(shiftstream.io.out.valid) && (in_s_write_cmpflag || !end_of_stream)

  io.bufs_completed := streams_completed
  io.no_writes_inflight := io.l2io.no_memops_inflight
}
