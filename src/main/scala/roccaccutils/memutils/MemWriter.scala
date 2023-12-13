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

class MemWriter(val metadataQueueDepth: Int = 10, val dataQueueDepth: Int = 4, printInfo: String = "", val writeCmpFlag: Boolean = true, val logger: Logger = DefaultLogger)(implicit p: Parameters, val hp: L2MemHelperParams)
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

  val incoming_writes_Q = Module(new Queue(new WriterBundle, metadataQueueDepth))
  incoming_writes_Q.io.enq <> io.memwrites_in

  val dest_info_Q = Module(new Queue(new DstInfo, metadataQueueDepth))
  dest_info_Q.io.enq <> io.dest_info

  val dest_last_fire = RegNext(dest_info_Q.io.deq.fire)
  val dest_last_valid = RegNext(dest_info_Q.io.deq.valid)
  val dest_printhelp = dest_info_Q.io.deq.valid && (dest_last_fire || (!dest_last_valid))

  LogUtils.logHexItems(
    dest_printhelp,
    Seq(
      ("op", dest_info_Q.io.deq.bits.op),
      ("cmpflag", dest_info_Q.io.deq.bits.cmpflag),
    ),
    Some("dest_info_q.deq"),
    oneline=true)

  val buf_lens_Q = Module(new Queue(UInt(64.W), 10))
  LogUtils.logHexItems(
    buf_lens_Q.io.enq.fire,
    Seq(
      ("buf_len", buf_lens_Q.io.enq.bits)
    ),
    Some("buf_lens_q.enq"),
    oneline=true)

  val buf_len_tracker = RegInit(0.U(64.W))
  when (incoming_writes_Q.io.deq.fire) {
    when (incoming_writes_Q.io.deq.bits.end_of_message) {
      buf_len_tracker := 0.U
    } .otherwise {
      buf_len_tracker := buf_len_tracker +& incoming_writes_Q.io.deq.bits.validbytes
    }
  }

  LogUtils.logHexItems(
    incoming_writes_Q.io.deq.fire,
    Seq(
      ("validbytes", incoming_writes_Q.io.deq.bits.validbytes),
      ("EOM", incoming_writes_Q.io.deq.bits.end_of_message),
      ("data", incoming_writes_Q.io.deq.bits.data),
    ),
    Some("incoming_writes_q.deq"),
    oneline=true)

  val NUM_QUEUES = BUS_SZ_BYTES
  val QUEUE_DEPTHS = dataQueueDepth
  val write_start_index = RegInit(0.U(log2Up(NUM_QUEUES+1).W))
  val mem_resp_queues = VecInit.fill(NUM_QUEUES)(Module(new Queue(UInt(8.W), QUEUE_DEPTHS)).io)
  // overridden below
  for (queueno <- 0 until NUM_QUEUES) {
    val q = mem_resp_queues(queueno)
    q.enq.valid := false.B
    q.enq.bits := 0.U
    q.deq.ready := false.B
  }

  val len_to_write = incoming_writes_Q.io.deq.bits.validbytes

  for (queueno <- 0 until NUM_QUEUES) {
    mem_resp_queues((write_start_index +& queueno.U) % NUM_QUEUES.U).enq.bits := incoming_writes_Q.io.deq.bits.data >> ((len_to_write - (queueno+1).U) << 3)
  }


  val wrap_len_index_wide = write_start_index +& len_to_write
  val wrap_len_index_end = wrap_len_index_wide % NUM_QUEUES.U
  val wrapped = wrap_len_index_wide >= NUM_QUEUES.U

  val all_queues_ready = mem_resp_queues.map(_.enq.ready).reduce(_ && _)


  val end_of_buf = incoming_writes_Q.io.deq.bits.end_of_message
  val account_for_buf_lens_Q = (!end_of_buf) || (end_of_buf && buf_lens_Q.io.enq.ready)

  val input_fire_allqueues = DecoupledHelper(
    incoming_writes_Q.io.deq.valid,
    all_queues_ready,
    account_for_buf_lens_Q
  )

  buf_lens_Q.io.enq.valid := input_fire_allqueues.fire(account_for_buf_lens_Q) && end_of_buf
  buf_lens_Q.io.enq.bits := buf_len_tracker +& incoming_writes_Q.io.deq.bits.validbytes

  incoming_writes_Q.io.deq.ready := input_fire_allqueues.fire(incoming_writes_Q.io.deq.valid)

  when (input_fire_allqueues.fire()) {
    write_start_index := wrap_len_index_end
  }

  for ( queueno <- 0 until NUM_QUEUES ) {
    val use_this_queue = Mux(wrapped,
                             (queueno.U >= write_start_index) || (queueno.U < wrap_len_index_end),
                             (queueno.U >= write_start_index) && (queueno.U < wrap_len_index_end)
                            )
    mem_resp_queues(queueno).enq.valid := input_fire_allqueues.fire() && use_this_queue
  }

  for ( queueno <- 0 until NUM_QUEUES ) {
    when (mem_resp_queues(queueno).deq.valid) {
      logger.logInfo("qi%d,0x%x\n", queueno.U, mem_resp_queues(queueno).deq.bits)
    }
  }

  val read_start_index = RegInit(0.U(log2Up(NUM_QUEUES+1).W))

  val remapVecData = Wire(Vec(NUM_QUEUES, UInt(8.W)))
  val remapVecValids = Wire(Vec(NUM_QUEUES, Bool()))
  val remapVecReadys = Wire(Vec(NUM_QUEUES, Bool()))

  for (queueno <- 0 until NUM_QUEUES) {
    val remapindex = (queueno.U +& read_start_index) % NUM_QUEUES.U
    remapVecData(queueno) := mem_resp_queues(remapindex).deq.bits
    remapVecValids(queueno) := mem_resp_queues(remapindex).deq.valid
    mem_resp_queues(remapindex).deq.ready := remapVecReadys(queueno)
  }

  val count_valids = remapVecValids.map(_.asUInt).reduce(_ +& _)

  val backend_bytes_written = RegInit(0.U(64.W))
  val backend_next_write_addr = dest_info_Q.io.deq.bits.op + backend_bytes_written

  val throttle_end = Mux(buf_lens_Q.io.deq.valid,
    buf_lens_Q.io.deq.bits - backend_bytes_written,
    BUS_SZ_BYTES.U)

  val throttle_end_writeable_meta = Seq((throttle_end >= BUS_SZ_BYTES.U) -> BUS_SZ_BYTES.U) ++
    (0 until BUS_SZ_BYTES_LG2UP).map(i => throttle_end(i) -> scala.math.pow(2,i).toInt.U).reverse
  val throttle_end_writeable_meta_log2 = Seq((throttle_end >= BUS_SZ_BYTES.U) -> BUS_SZ_BYTES_LG2UP.U) ++
    (0 until BUS_SZ_BYTES_LG2UP).map(i => throttle_end(i) -> i.U).reverse
  val throttle_end_writeable = MuxCase(0.U, throttle_end_writeable_meta)
  val throttle_end_writeable_log2 = MuxCase(0.U, throttle_end_writeable_meta_log2)

  val ptr_align_max_bytes_writable_meta = (0 until BUS_SZ_BYTES_LG2UP).map(i => backend_next_write_addr(i) -> scala.math.pow(2,i).toInt.U)
  val ptr_align_max_bytes_writable_meta_log2 = (0 until BUS_SZ_BYTES_LG2UP).map(i => backend_next_write_addr(i) -> i.U)
  val ptr_align_max_bytes_writeable = MuxCase(BUS_SZ_BYTES.U, ptr_align_max_bytes_writable_meta)
  val ptr_align_max_bytes_writeable_log2 = MuxCase(BUS_SZ_BYTES_LG2UP.U, ptr_align_max_bytes_writable_meta_log2)

  val count_valids_meta = (0 to BUS_SZ_BYTES_LG2UP).map(i => count_valids(i) -> scala.math.pow(2,i).toInt.U).reverse
  val count_valids_meta_log2 = (0 to BUS_SZ_BYTES_LG2UP).map(i => count_valids(i) -> i.U).reverse
  val count_valids_largest_aligned = MuxCase(0.U, count_valids_meta)
  val count_valids_largest_aligned_log2 = MuxCase(0.U, count_valids_meta_log2)

  val bytes_to_write = Mux(
    ptr_align_max_bytes_writeable < count_valids_largest_aligned,
    Mux(ptr_align_max_bytes_writeable < throttle_end_writeable,
      ptr_align_max_bytes_writeable,
      throttle_end_writeable),
    Mux(count_valids_largest_aligned < throttle_end_writeable,
      count_valids_largest_aligned,
      throttle_end_writeable)
  )
  val bytes_to_write_log2 = Mux(
    ptr_align_max_bytes_writeable_log2 < count_valids_largest_aligned_log2,
    Mux(ptr_align_max_bytes_writeable_log2 < throttle_end_writeable_log2,
      ptr_align_max_bytes_writeable_log2,
      throttle_end_writeable_log2),
    Mux(count_valids_largest_aligned_log2 < throttle_end_writeable_log2,
      count_valids_largest_aligned_log2,
      throttle_end_writeable_log2)
  )

  val enough_data = bytes_to_write =/= 0.U

  val done_writing_stream = buf_lens_Q.io.deq.valid && (buf_lens_Q.io.deq.bits === backend_bytes_written)

  val mem_write_fire = DecoupledHelper(
    io.l2io.req.ready,
    enough_data,
    !done_writing_stream,
    dest_info_Q.io.deq.valid
  )

  val bool_ptr_write_fire = DecoupledHelper(
    io.l2io.req.ready,
    buf_lens_Q.io.deq.valid,
    buf_lens_Q.io.deq.bits === backend_bytes_written,
    dest_info_Q.io.deq.valid
  )

  for (queueno <- 0 until NUM_QUEUES) {
    remapVecReadys(queueno) := (queueno.U < bytes_to_write) && mem_write_fire.fire()
  }

  when (mem_write_fire.fire()) {
    read_start_index := (read_start_index +& bytes_to_write) % NUM_QUEUES.U
    backend_bytes_written := backend_bytes_written + bytes_to_write

  }

  val remapped_write_data = Cat(remapVecData.reverse) // >> ((NUM_QUEUES.U - bytes_to_write) << 3)

  if (writeCmpFlag) {
    io.l2io.req.valid := mem_write_fire.fire(io.l2io.req.ready) || bool_ptr_write_fire.fire(io.l2io.req.ready)
  } else {
    io.l2io.req.valid := mem_write_fire.fire(io.l2io.req.ready)
  }
  io.l2io.req.bits.size := Mux(done_writing_stream, 0.U, bytes_to_write_log2)
  io.l2io.req.bits.addr := Mux(done_writing_stream, dest_info_Q.io.deq.bits.cmpflag, backend_next_write_addr)
  io.l2io.req.bits.data := Mux(done_writing_stream, 1.U, remapped_write_data)
  io.l2io.req.bits.cmd := M_XWR

  LogUtils.logHexItems(
    io.l2io.req.fire,
    Seq(
      ("addr", io.l2io.req.bits.addr),
      ("size", io.l2io.req.bits.size),
      ("data", io.l2io.req.bits.data),
    ),
    Some("L2IO write fire"),
    oneline=true)

  buf_lens_Q.io.deq.ready := bool_ptr_write_fire.fire(buf_lens_Q.io.deq.valid)
  dest_info_Q.io.deq.ready := bool_ptr_write_fire.fire(dest_info_Q.io.deq.valid)

  val bufs_completed = RegInit(0.U(64.W))
  io.bufs_completed := bufs_completed

  io.l2io.resp.ready := true.B

  io.no_writes_inflight := io.l2io.no_memops_inflight

  when (bool_ptr_write_fire.fire()) {
    bufs_completed := bufs_completed + 1.U
    backend_bytes_written := 0.U
  }

  LogUtils.logHexItems(
    bool_ptr_write_fire.fire,
    Seq(
      ("write_cmpflag", dest_info_Q.io.deq.bits.cmpflag),
      ("write_ptr", dest_info_Q.io.deq.bits.op),
    ),
    Some("dest_info_q.deq"),
    oneline=true)

  LogUtils.logHexItems(
    (count_valids =/= 0.U),
    Seq(
      ("write_start_index", read_start_index),
      ("backend_bytes_written", backend_bytes_written),
      ("count_valids", count_valids),
      ("ptr_align_max_bytes_writeable", ptr_align_max_bytes_writeable),
      ("bytes_to_write", bytes_to_write),
      ("bytes_to_write_log2", bytes_to_write_log2),
    ),
    Some("memwriter-serializer"))
}
