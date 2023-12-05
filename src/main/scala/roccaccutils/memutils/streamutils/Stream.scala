// See LICENSE for license details

package roccaccutils.memutils

import chisel3._
import chisel3.util._

// These classes originated in testchipip, but are copied
// here to reduce dependencies (modified to be numbytes instead of keep - i.e. valid data always aligned to 0th idx)
class StreamChannel(val w: Int) extends Bundle {
  val wL2Up = log2Up(w) + 1

  val data = UInt(w.W)
  val numbytes = UInt(wL2Up.W)
  val last = Bool()
}

class StreamIO(val w: Int) extends Bundle {
  val in = Flipped(Decoupled(new StreamChannel(w)))
  val out = Decoupled(new StreamChannel(w))

  def flipConnect(other: StreamIO) {
    in <> other.out
    other.in <> out
  }
}
