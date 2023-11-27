// See LICENSE for license details

package roccaccutils.memutils

import chisel3._
import chisel3.util._

// This class originated in icenet, but is copied
// here to reduce dependencies
class Aligner(dataBits: Int) extends Module {
  val dataBytes = dataBits / 8

  val io = IO(new StreamIO(dataBits))

  val data = RegInit(0.U(dataBits.W))
  val keep = RegInit(0.U(dataBytes.W))
  val last = RegInit(false.B)
  val nbytes = RegInit(0.U(log2Ceil(dataBytes + 1).W))

  assert(!io.in.valid || io.in.bits.keep.orR,
    "Aligner cannot handle an empty flit")

  val rshift = PriorityEncoder(io.in.bits.keep)
  val full_keep = ((io.in.bits.keep >> rshift) << nbytes) | keep

  val in_mask = FillInterleaved(8, io.in.bits.keep)
  val in_data = io.in.bits.data & in_mask

  val rshift_bit = Cat(rshift, 0.U(3.W))
  val nbits = Cat(nbytes, 0.U(3.W))
  val bitmask = FillInterleaved(8, keep)
  val full_data = ((in_data >> rshift_bit) << nbits) | (data & bitmask)
  val full_nbytes = PopCount(full_keep)
  val fwd_last = io.in.bits.last && (full_keep >> dataBytes.U) === 0.U

  io.out.valid := (last && nbytes > 0.U) ||
                  (io.in.valid && (fwd_last || full_nbytes >= dataBytes.U))
  io.out.bits.data := Mux(last, data, full_data(dataBits-1, 0))
  io.out.bits.keep := Mux(last, keep, full_keep(dataBytes-1, 0))
  io.out.bits.last := last || fwd_last

  io.in.ready := full_nbytes < dataBytes.U ||
                 (io.out.ready && !last)

  when (io.in.fire && io.out.fire) {
    data := full_data >> dataBits.U
    keep := full_keep >> dataBytes.U
    last := io.in.bits.last && !fwd_last
    nbytes := Mux(fwd_last, 0.U, full_nbytes - dataBytes.U)
  } .elsewhen (io.in.fire) {
    data := full_data
    keep := full_keep
    last := io.in.bits.last
    nbytes := full_nbytes
  } .elsewhen (io.out.fire) {
    data := 0.U
    keep := 0.U
    last := false.B
    nbytes := 0.U
  }
}
