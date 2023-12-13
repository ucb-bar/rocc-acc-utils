// See LICENSE for license details

package roccaccutils.memstreamer

import chisel3._
import chisel3.util._

import org.chipsalliance.cde.config.{Parameters}
import freechips.rocketchip.util.{DecoupledHelper}
import freechips.rocketchip.diplomacy.{ValName}
import roccaccutils.logger._
import roccaccutils.memutils._

class LiteralChunk(implicit val hp: L2MemHelperParams) extends Bundle with HasL2MemHelperParams {
  val chunk_data = UInt(BUS_SZ_BITS.W)
  val chunk_size_bytes = UInt((BUS_SZ_BYTES_LG2UP + 1).W)
  val is_final_chunk = Bool()
}

class MemStreamerBundle(implicit hp: L2MemHelperParams) extends Bundle {
  val mem_stream = Flipped(new MemLoaderConsumerBundle) //from MemLoader
  val memwrites_in = Decoupled(new WriterBundle) //to MemWriter
}

trait MemStreamer
  extends Module
  with HasL2MemHelperParams {

  // --------------------------
  // MUST BE DEFINED BY CHILD
  // --------------------------

  val io: MemStreamerBundle
  implicit val p: Parameters
  implicit val valName: ValName = ValName("<MemStreamer>")
  val logger: Logger

  // --------------------------

  // 1. Receive data from the memloader (i.e. mem_stream)

  // 2. Write data to through the memwriter (i.e. store_data_queue)

  val store_data_queue = Module(new Queue(new LiteralChunk, 5))
  dontTouch(store_data_queue.io.count)

  val sdq_chunk_size = store_data_queue.io.deq.bits.chunk_size_bytes
  val sdq_chunk_data = store_data_queue.io.deq.bits.chunk_data
  val sdq_chunk_data_vec = VecInit(Seq.fill(BUS_SZ_BYTES)(0.U(8.W)))
  for (i <- 0 to (BUS_SZ_BYTES - 1)) {
    sdq_chunk_data_vec(sdq_chunk_size - 1.U - i.U) := sdq_chunk_data((8*(i+1))-1, 8*i)
  }
  io.memwrites_in.bits.data := sdq_chunk_data_vec.asUInt
  io.memwrites_in.bits.validbytes := sdq_chunk_size
  io.memwrites_in.bits.end_of_message := store_data_queue.io.deq.bits.is_final_chunk
  io.memwrites_in.valid := store_data_queue.io.deq.valid
  store_data_queue.io.deq.ready := io.memwrites_in.ready
}
