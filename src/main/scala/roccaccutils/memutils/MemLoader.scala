// See LICENSE for license details

package roccaccutils.memutils

import chisel3._
import chisel3.util._

import org.chipsalliance.cde.config.{Parameters}
import freechips.rocketchip.util.{DecoupledHelper}
import freechips.rocketchip.rocket.constants.{MemoryOpConstants}
import roccaccutils.logger._

class MemLoaderConsumerBundle(implicit val hp: L2MemHelperParams) extends Bundle with HasL2MemHelperParams {
  val user_consumed_bytes = Input(UInt((BUS_SZ_BYTES_LG2UP + 1).W)) // amt of bytes read (comb. related to available_output_bytes)
  val available_output_bytes = Output(UInt((BUS_SZ_BYTES_LG2UP + 1).W))
  val output_valid = Output(Bool())
  val output_ready = Input(Bool())
  val output_data = Output(UInt(BUS_SZ_BITS.W))
  val output_last_chunk = Output(Bool())
}

class StreamInfo extends Bundle {
  val ip = UInt(64.W) // input pointer
  val isize = UInt(64.W)
}

// API:
//   Output available in bus bytes sized chunks that the user can read out
//     - is aligned, and user can read it in any size chunk
//     - user must read out all data for it to not stall
//
//   Input given in bus bytes sized chunks, may not be aligned
class MemLoader(metadataQueueDepth: Int = 10, dataQueueDepth: Int = 16*4, logger: Logger = DefaultLogger)(implicit p: Parameters, val hp: L2MemHelperParams) extends Module
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
  val base_addr_bytes = io.src_info.bits.ip
  val base_len = io.src_info.bits.isize
  // start byte of first word to load
  val base_addr_start_index = base_addr_bytes & BUS_BYTE_LG2UP_MASK
  val aligned_loadlen = base_len + base_addr_start_index
  val base_addr_end_index = aligned_loadlen & BUS_BYTE_LG2UP_MASK
  val has_extra_word = (base_addr_end_index =/= 0.U)
  // end byte of final word to load (exclusive)
  val base_addr_end_index_exclusive = Mux(has_extra_word, base_addr_end_index, BUS_SZ_BYTES.U)

  val base_addr_bytes_aligned = (base_addr_bytes >> BUS_SZ_BYTES_LG2UP.U) << BUS_SZ_BYTES_LG2UP.U
  val words_to_load = (aligned_loadlen >> BUS_SZ_BYTES_LG2UP.U) + has_extra_word.asUInt
  val words_to_load_minus_one = words_to_load - 1.U

  LogUtils.logHexItems(
    io.src_info.fire,
    Seq(
      ("base_addr_bytes", base_addr_bytes),
      ("base_len", base_len),
      ("base_addr_start_index", base_addr_start_index),
      ("aligned_loadlen", aligned_loadlen),
      ("base_addr_end_index", base_addr_end_index),
      ("base_addr_end_index_exclusive", base_addr_end_index_exclusive),
      ("has_extra_word", has_extra_word),
      ("base_addr_bytes_aligned", base_addr_bytes_aligned),
      ("words_to_load", words_to_load),
      ("words_to_load_minus_one", words_to_load_minus_one),
    ),
    Some("Completed src_info load"),
    logger=logger)

  val load_info_queue = Module(new Queue(new Bundle {
    val start_byte = UInt(BUS_SZ_BYTES_LG2UP.W) // inclusive
    val end_byte = UInt((BUS_SZ_BYTES_LG2UP + 1).W) // exclusive
    val last = Bool()
  }, metadataQueueDepth)) // arb. size

  val req_fire = DecoupledHelper(
    io.l2io.req.ready,
    io.src_info.valid,
    load_info_queue.io.enq.ready
  )

  val addrinc = RegInit(0.U(64.W))
  val is_first_word = (addrinc === 0.U)
  val is_last_word = (addrinc === words_to_load_minus_one)

  when (req_fire.fire()) {
    addrinc := Mux(is_last_word, 0.U, addrinc + 1.U)
  }

  io.src_info.ready := req_fire.fire(io.src_info.valid, is_last_word)

  load_info_queue.io.enq.valid := req_fire.fire(load_info_queue.io.enq.ready)
  load_info_queue.io.enq.bits.start_byte := Mux(is_first_word, base_addr_start_index, 0.U)
  load_info_queue.io.enq.bits.end_byte := Mux(is_last_word, base_addr_end_index_exclusive, BUS_SZ_BYTES.U)
  load_info_queue.io.enq.bits.last := req_fire.fire() && is_last_word

  io.l2io.req.valid := req_fire.fire(io.l2io.req.ready)
  io.l2io.req.bits.addr := (base_addr_bytes_aligned) + (addrinc << BUS_SZ_BYTES_LG2UP)
  io.l2io.req.bits.cmd := M_XRD
  io.l2io.req.bits.size := BUS_SZ_BYTES_LG2UP.U
  io.l2io.req.bits.data := 0.U

  LogUtils.logHexItems(
    load_info_queue.io.deq.fire,
    Seq(
      ("start_byte", load_info_queue.io.deq.bits.start_byte),
      ("end_byte", load_info_queue.io.deq.bits.end_byte),
      ("last", load_info_queue.io.deq.bits.last),
    ),
    Some("load_info_queue.deq"),
    oneline=true,
    logger=logger)

  val resp_fire = DecoupledHelper(
    io.l2io.resp.valid,
    load_info_queue.io.deq.valid
  )
  load_info_queue.io.deq.ready := resp_fire.fire(load_info_queue.io.deq.valid)
  io.l2io.resp.ready := resp_fire.fire(io.l2io.resp.valid)

  // align data
  val aligner = Module(new Aligner(BUS_SZ_BITS))
  aligner.io.in.valid := resp_fire.fire()
  aligner.io.in.bits.data := io.l2io.resp.bits.data
  aligner.io.in.bits.last := load_info_queue.io.deq.bits.last
  aligner.io.in.bits.keep := (BUS_BYTE_MASK >> (BUS_SZ_BYTES.U - load_info_queue.io.deq.bits.end_byte)) & (BUS_BYTE_MASK << load_info_queue.io.deq.bits.start_byte)

  // read out data, shifting based on user input
  val shiftstream = Module(new StreamShifter(BUS_SZ_BITS, dataQueueDepth))
  shiftstream.io.in.valid := aligner.io.out.valid
  aligner.io.out.ready := shiftstream.io.in.ready
  shiftstream.io.in.bits.data := aligner.io.out.bits.data
  shiftstream.io.in.bits.keep := aligner.io.out.bits.keep
  shiftstream.io.in.bits.last := aligner.io.out.bits.last

  shiftstream.io.out.bits.read_bytes := io.consumer.user_consumed_bytes
  io.consumer.available_output_bytes := shiftstream.io.out.bits.bytes_avail
  io.consumer.output_valid := shiftstream.io.out.valid
  shiftstream.io.out.ready := io.consumer.output_ready
  io.consumer.output_data := shiftstream.io.out.bits.data
  io.consumer.output_last_chunk := shiftstream.io.out.bits.last
}
