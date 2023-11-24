// See LICENSE for license details

package roccaccutils

import chisel3._
import chisel3.util._

import org.chipsalliance.cde.config.{Parameters}
import freechips.rocketchip.util.{DecoupledHelper}
import freechips.rocketchip.rocket.constants.{MemoryOpConstants}
import roccaccutils.logger._
import icenet.{Aligner, StreamChannel}

class MemLoader(memLoaderQueDepth: Int = 16*4, logger: Logger = DefaultLogger)(implicit p: Parameters, val hp: L2MemHelperParams) extends Module
     with MemoryOpConstants
     with HasL2MemHelperParams {

  val io = IO(new Bundle {
    val l2helperUser = new L2MemHelperBundle
    val src_info = Flipped(Decoupled(new StreamInfo))
    val consumer = new MemLoaderConsumerBundle
  })

  val load_info_queue = Module(new Queue(new Bundle {
    val load_info = new LoadInfoBundle
    val last = Bool()
  }, 256))

  val base_addr_bytes = io.src_info.bits.ip
  val base_len = io.src_info.bits.isize
  val base_addr_start_index = io.src_info.bits.ip & BUS_BYTE_LG2UP_MASK
  val aligned_loadlen =  base_len + base_addr_start_index
  val base_addr_end_index = (base_len + base_addr_start_index) & BUS_BYTE_LG2UP_MASK
  val base_addr_end_index_inclusive = (base_len + base_addr_start_index - 1.U) & BUS_BYTE_LG2UP_MASK
  val extra_word = ((aligned_loadlen & BUS_BYTE_LG2UP_MASK) =/= 0.U).asUInt

  val base_addr_bytes_aligned = (base_addr_bytes >> BUS_SZ_BYTES_LG2UP.U) << BUS_SZ_BYTES_LG2UP.U
  val words_to_load = (aligned_loadlen >> BUS_SZ_BYTES_LG2UP.U) + extra_word
  val words_to_load_minus_one = words_to_load - 1.U

  val print_not_done = RegInit(true.B)
  when (io.src_info.valid && print_not_done) {
    logger.logInfo("base_addr_bytes: %x\n", base_addr_bytes)
    logger.logInfo("base_len: %x\n", base_len)
    logger.logInfo("base_addr_start_index: %x\n", base_addr_start_index)
    logger.logInfo("aligned_loadlen: %x\n", aligned_loadlen)
    logger.logInfo("base_addr_end_index: %x\n", base_addr_end_index)
    logger.logInfo("base_addr_end_index_inclusive: %x\n", base_addr_end_index_inclusive)
    logger.logInfo("extra_word: %x\n", extra_word)
    logger.logInfo("base_addr_bytes_aligned: %x\n", base_addr_bytes_aligned)
    logger.logInfo("words_to_load: %x\n", words_to_load)
    logger.logInfo("words_to_load_minus_one: %x\n", words_to_load_minus_one)
    when (io.src_info.ready) {
      print_not_done := true.B
    } .otherwise {
      print_not_done := false.B
    }
  }
  when (io.src_info.fire) {
    logger.logInfo("COMPLETED SRC INPUT LOAD\n")
  }

  val request_fire = DecoupledHelper(
    io.l2helperUser.req.ready,
    io.src_info.valid,
    load_info_queue.io.enq.ready
  )

  io.l2helperUser.req.bits.cmd := M_XRD
  io.l2helperUser.req.bits.size := BUS_SZ_BYTES_LG2UP.U
  io.l2helperUser.req.bits.data := 0.U

  val addrinc = RegInit(0.U(64.W))

  load_info_queue.io.enq.bits.load_info.start_byte := Mux(addrinc === 0.U, base_addr_start_index, 0.U)
  load_info_queue.io.enq.bits.load_info.end_byte := Mux(addrinc === words_to_load_minus_one, base_addr_end_index_inclusive, 31.U)

  when (request_fire.fire() && (addrinc === words_to_load_minus_one)) {
    addrinc := 0.U
  } .elsewhen (request_fire.fire()) {
    addrinc := addrinc + 1.U
  }

  io.src_info.ready := request_fire.fire(io.src_info.valid, addrinc === words_to_load_minus_one)

  load_info_queue.io.enq.valid := request_fire.fire(load_info_queue.io.enq.ready)
  load_info_queue.io.enq.bits.last := request_fire.fire() && (addrinc === 0.U)

  io.l2helperUser.req.bits.addr := (base_addr_bytes_aligned) + (addrinc << BUS_SZ_BYTES_LG2UP)
  io.l2helperUser.req.valid := request_fire.fire(io.l2helperUser.req.ready)

  when (load_info_queue.io.deq.fire) {
    logger.logInfo("load_info_queue: start %x, end %x, last %x\n",
      load_info_queue.io.deq.bits.load_info.start_byte,
      load_info_queue.io.deq.bits.load_info.end_byte,
      load_info_queue.io.deq.bits.last)
  }

  // API:
  //   Output available in bus bytes sized chunks that the user can read out
  //     - is aligned, and user can read it in any size chunk
  //
  //   Input given in bus bytes sized chunks, may not be aligned

  val resp_fire_noqueue = DecoupledHelper(
    io.l2helperUser.resp.valid,
    load_info_queue.io.deq.valid
  )
  load_info_queue.io.deq.ready := resp_fire_noqueue.fire(load_info_queue.io.deq.valid)
  io.l2helperUser.resp.ready := resp_fire_noqueue.fire(io.l2helperUser.resp.valid)

  // align data
  val aligner = Module(new Aligner(BUS_SZ_BITS))
  aligner.io.in.valid := resp_fire_noqueue.fire()
  aligner.io.in.bits.data := io.l2helperUser.resp.bits.data
  aligner.io.in.bits.last := load_info_queue.io.deq.bits.last
  aligner.io.in.bits.keep := BUS_BYTE_MASK >> load_info_queue.io.deq.bits.load_info.start_byte

  // store aligned data
  val aligned_data_queue = Module(new Queue(new StreamChannel(BUS_SZ_BITS), memLoaderQueDepth))
  aligned_data_queue.io.enq <> aligner.io.out

  // read out data, shifting based on user input
  val shiftstream = Module(new StreamShifter(BUS_SZ_BITS, 2*BUS_SZ_BITS))
  shiftstream.io.in.valid := aligned_data_queue.io.deq.valid
  aligned_data_queue.io.deq.ready := shiftstream.io.in.ready
  shiftstream.io.in.bits.data := aligned_data_queue.io.deq.bits.data
  shiftstream.io.in.bits.keep := aligned_data_queue.io.deq.bits.keep
  shiftstream.io.in.bits.last := aligned_data_queue.io.deq.bits.last

  shiftstream.io.out.bits.read_bytes := io.consumer.user_consumed_bytes
  io.consumer.available_output_bytes := shiftstream.io.out.bits.bytes_avail
  io.consumer.output_valid := shiftstream.io.out.valid
  shiftstream.io.out.ready := io.consumer.output_ready
  io.consumer.output_data := shiftstream.io.out.bits.data
  io.consumer.output_last_chunk := shiftstream.io.out.bits.last
}
