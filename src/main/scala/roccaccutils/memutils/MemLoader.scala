// See LICENSE for license details

package roccaccutils.memutils

import chisel3._
import chisel3.util._

import org.chipsalliance.cde.config.{Parameters}
import freechips.rocketchip.util.{DecoupledHelper}
import freechips.rocketchip.rocket.constants.{MemoryOpConstants}
import roccaccutils.logger._

class MemLoaderConsumerBundle(implicit val hp: L2MemHelperParams) extends Bundle with HasL2MemHelperParams {
  val req = Flipped(Decoupled(UInt((BUS_SZ_BYTES_LG2UP + 1).W))) // amt of bytes requested
  val output_data = Output(UInt(BUS_SZ_BITS.W))
  val output_bytes_avail = Output(UInt((BUS_SZ_BYTES_LG2UP + 1).W))
  val output_last_chunk = Output(Bool())
}

class StreamInfo extends Bundle {
  val ip = UInt(64.W) // input pointer
  val isize = UInt(64.W) // input size
}

// API:
//   Output available in bus bytes sized chunks that the user can read out
//     - is aligned, and user can read it in any size chunk
//     - user must read out all data for it to not stall
//
//   Input given in bus bytes sized chunks, may not be aligned
class MemLoader(metadataQueueDepth: Int = 10, dataQueueDepth: Int = 4, printInfo: String = "", logger: Logger = DefaultLogger)(implicit p: Parameters, val hp: L2MemHelperParams) extends Module
     with MemoryOpConstants
     with HasL2MemHelperParams {

  val io = IO(new Bundle {
    val l2io = new L2MemHelperBundle

    val src_info = Flipped(Decoupled(new StreamInfo))
    val consumer = new MemLoaderConsumerBundle
  })

  when (io.src_info.fire) {
    assert(io.src_info.bits.isize =/= 0.U, "Cannot load 0 bytes")
  }

  class BufInfoBundle extends Bundle {
    val len_bytes = UInt(64.W)
  }

  class LoadInfoBundle(implicit val hp: L2MemHelperParams) extends Bundle with HasL2MemHelperParams {
    val start_byte = UInt(BUS_SZ_BYTES_LG2UP.W)
    val end_byte = UInt(BUS_SZ_BYTES_LG2UP.W)
  }

  val buf_info_queue = Module(new Queue(new BufInfoBundle, metadataQueueDepth))

  val load_info_queue = Module(new Queue(new LoadInfoBundle, metadataQueueDepth))

  val base_addr_bytes = io.src_info.bits.ip
  val base_len = io.src_info.bits.isize
  val base_addr_start_index = io.src_info.bits.ip & BUS_BIT_MASK
  val aligned_loadlen =  base_len + base_addr_start_index
  val base_addr_end_index = (base_len + base_addr_start_index) & BUS_BIT_MASK
  val base_addr_end_index_inclusive = (base_len + base_addr_start_index - 1.U) & BUS_BIT_MASK
  val extra_word = ((aligned_loadlen & BUS_BIT_MASK) =/= 0.U).asUInt

  val base_addr_bytes_aligned = (base_addr_bytes >> BUS_SZ_BYTES_LG2UP.U) << BUS_SZ_BYTES_LG2UP.U
  val words_to_load = (aligned_loadlen >> BUS_SZ_BYTES_LG2UP.U) + extra_word
  val words_to_load_minus_one = words_to_load - 1.U

  val print_not_done = RegInit(true.B)

  when (io.src_info.valid && print_not_done) {
    logger.logInfo(printInfo + ": base_addr_bytes: %x\n", base_addr_bytes)
    logger.logInfo(printInfo + ": base_len: %x\n", base_len)
    logger.logInfo(printInfo + ": base_addr_start_index: %x\n", base_addr_start_index)
    logger.logInfo(printInfo + ": aligned_loadlen: %x\n", aligned_loadlen)
    logger.logInfo(printInfo + ": base_addr_end_index: %x\n", base_addr_end_index)
    logger.logInfo(printInfo + ": base_addr_end_index_inclusive: %x\n", base_addr_end_index_inclusive)
    logger.logInfo(printInfo + ": extra_word: %x\n", extra_word)
    logger.logInfo(printInfo + ": base_addr_bytes_aligned: %x\n", base_addr_bytes_aligned)
    logger.logInfo(printInfo + ": words_to_load: %x\n", words_to_load)
    logger.logInfo(printInfo + ": words_to_load_minus_one: %x\n", words_to_load_minus_one)
    when (io.src_info.ready) {
      print_not_done := true.B
    } .otherwise {
      print_not_done := false.B
    }
  }

  val request_fire = DecoupledHelper(
    io.l2io.req.ready,
    io.src_info.valid,
    buf_info_queue.io.enq.ready,
    load_info_queue.io.enq.ready
  )

  io.l2io.req.bits.cmd := M_XRD
  io.l2io.req.bits.size := BUS_SZ_BYTES_LG2UP.U
  io.l2io.req.bits.data := 0.U

  val addrinc = RegInit(0.U(64.W))

  load_info_queue.io.enq.bits.start_byte := Mux(addrinc === 0.U, base_addr_start_index, 0.U)
  load_info_queue.io.enq.bits.end_byte := Mux(addrinc === words_to_load_minus_one, base_addr_end_index_inclusive, 31.U)


  when (request_fire.fire() && (addrinc === words_to_load_minus_one)) {
    addrinc := 0.U
  } .elsewhen (request_fire.fire()) {
    addrinc := addrinc + 1.U
  }

  when (io.src_info.fire) {
    logger.logInfo(printInfo + ": COMPLETED INPUT LOAD\n")
  }


  io.src_info.ready := request_fire.fire(io.src_info.valid,
                                            addrinc === words_to_load_minus_one)

  buf_info_queue.io.enq.valid := request_fire.fire(buf_info_queue.io.enq.ready,
                                            addrinc === 0.U)
  load_info_queue.io.enq.valid := request_fire.fire(load_info_queue.io.enq.ready)

  buf_info_queue.io.enq.bits.len_bytes := base_len

  io.l2io.req.bits.addr := (base_addr_bytes_aligned) + (addrinc << BUS_SZ_BYTES_LG2UP)
  io.l2io.req.valid := request_fire.fire(io.l2io.req.ready)

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

  val align_shamt = (load_info_queue.io.deq.bits.start_byte << 3)
  val memresp_bits_shifted = io.l2io.resp.bits.data >> align_shamt

  for ( queueno <- 0 until NUM_QUEUES ) {
    mem_resp_queues((write_start_index +& queueno.U) % NUM_QUEUES.U).enq.bits := memresp_bits_shifted >> (queueno * 8)
  }

  val len_to_write = (load_info_queue.io.deq.bits.end_byte - load_info_queue.io.deq.bits.start_byte) +& 1.U

  val wrap_len_index_wide = write_start_index +& len_to_write
  val wrap_len_index_end = wrap_len_index_wide % NUM_QUEUES.U
  val wrapped = wrap_len_index_wide >= NUM_QUEUES.U

  when (load_info_queue.io.deq.valid) {
    logger.logInfo(printInfo + ": memloader start %x, end %x\n", load_info_queue.io.deq.bits.start_byte,
      load_info_queue.io.deq.bits.end_byte)
  }

  val resp_fire_noqueues = DecoupledHelper(
    io.l2io.resp.valid,
    load_info_queue.io.deq.valid
  )
  val all_queues_ready = mem_resp_queues.map(_.enq.ready).reduce(_ && _)

  load_info_queue.io.deq.ready := resp_fire_noqueues.fire(load_info_queue.io.deq.valid, all_queues_ready)
  io.l2io.resp.ready := resp_fire_noqueues.fire(io.l2io.resp.valid, all_queues_ready)

  val resp_fire_allqueues = resp_fire_noqueues.fire() && all_queues_ready
  when (resp_fire_allqueues) {
    write_start_index := wrap_len_index_end
  }

  for ( queueno <- 0 until NUM_QUEUES ) {
    val use_this_queue = Mux(wrapped,
                             (queueno.U >= write_start_index) || (queueno.U < wrap_len_index_end),
                             (queueno.U >= write_start_index) && (queueno.U < wrap_len_index_end)
                            )
    mem_resp_queues(queueno).enq.valid := resp_fire_noqueues.fire() && use_this_queue && all_queues_ready
  }

  for ( queueno <- 0 until NUM_QUEUES ) {
    when (mem_resp_queues(queueno).deq.valid) {
      logger.logInfo(printInfo + ": queueind %d, val %x\n", queueno.U, mem_resp_queues(queueno).deq.bits)
    }
  }

  val read_start_index = RegInit(0.U(log2Up(NUM_QUEUES+1).W))

  val len_already_consumed = RegInit(0.U(64.W))

  val remapVecData = Wire(Vec(NUM_QUEUES, UInt(8.W)))
  val remapVecValids = Wire(Vec(NUM_QUEUES, Bool()))
  val remapVecReadys = Wire(Vec(NUM_QUEUES, Bool()))

  for (queueno <- 0 until NUM_QUEUES) {
    val remapindex = (queueno.U +& read_start_index) % NUM_QUEUES.U
    remapVecData(queueno) := mem_resp_queues(remapindex).deq.bits
    remapVecValids(queueno) := mem_resp_queues(remapindex).deq.valid
    mem_resp_queues(remapindex).deq.ready := remapVecReadys(queueno)
  }
  io.consumer.output_data := Cat(remapVecData.reverse)


  val buf_last = (len_already_consumed + io.consumer.req.bits) === buf_info_queue.io.deq.bits.len_bytes
  val count_valids = remapVecValids.map(_.asUInt).reduce(_ +& _)
  val unconsumed_bytes_so_far = buf_info_queue.io.deq.bits.len_bytes - len_already_consumed

  val enough_data = Mux(unconsumed_bytes_so_far >= NUM_QUEUES.U,
                        count_valids === NUM_QUEUES.U,
                        count_valids >= unconsumed_bytes_so_far)

  io.consumer.output_bytes_avail := Mux(unconsumed_bytes_so_far >= NUM_QUEUES.U,
                                    NUM_QUEUES.U,
                                    unconsumed_bytes_so_far)

  io.consumer.output_last_chunk := (unconsumed_bytes_so_far <= NUM_QUEUES.U)

  val read_fire = DecoupledHelper(
    io.consumer.req.valid,
    buf_info_queue.io.deq.valid,
    enough_data
  )

  when (read_fire.fire()) {
    logger.logInfo(printInfo + ": MEMLOADER READ: bytesread %d\n", io.consumer.req.bits)

  }

  io.consumer.req.ready := read_fire.fire(io.consumer.req.valid)

  for (queueno <- 0 until NUM_QUEUES) {
    remapVecReadys(queueno) := (queueno.U < io.consumer.req.bits) && read_fire.fire()
  }

  when (read_fire.fire()) {
    read_start_index := (read_start_index +& io.consumer.req.bits) % NUM_QUEUES.U
  }

  buf_info_queue.io.deq.ready := read_fire.fire(buf_info_queue.io.deq.valid) && buf_last

  when (read_fire.fire()) {
    when (buf_last) {
      len_already_consumed := 0.U
    } .otherwise {
      len_already_consumed := len_already_consumed + io.consumer.req.bits
    }
  }
}
