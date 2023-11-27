// See LICENSE for license details

package roccaccutils.logger

import chisel3._

import org.chipsalliance.cde.config.{Parameters}
import freechips.rocketchip.diplomacy.{ValName}

object LogUtils {
  def logHexItems(
    cond: Bool,
    items: Seq[(String, Bits)],
    header: Option[String] = None,
    oneline: Boolean = false,
    critical: Boolean = false,
    logger: Logger = DefaultLogger
  )(
    implicit p: Parameters = Parameters.empty,
    valName: ValName = ValName("<UnknownMod>")
  ): Unit = {
    require(items.length > 0, "Must print at least one item")
    when (cond) {
      val itemSep = if (oneline) ", " else "\n"
      val prefixSep = if (oneline) "" else "  "
      val itemStrs = items.map(prefixSep + _._1 + ":0x%x")
      val finalItemStr = itemStrs.mkString(itemSep)

      val headerStr = if (header.nonEmpty) header.get + ": " else ""
      val headerSep = if (oneline) "" else "\n"
      val finalHeaderStr = headerStr + headerSep

      val finalStr = finalHeaderStr + finalItemStr + "\n"
      val bits = items.map(_._2)

      if (critical) {
        logger.logCritical(finalStr, bits:_*)
      } else {
        logger.logInfo(finalStr, bits:_*)
      }
    }
  }

  def logAndAssert(
    cond: Bool,
    assertStr: String,
    critical: Boolean = false,
    logger: Logger = DefaultLogger
  )(
    implicit p: Parameters = Parameters.empty,
    valName: ValName = ValName("<UnknownMod>")
  ): Unit = {
    when (!cond) {
      if (critical) {
        logger.logCritical(assertStr)
      } else {
        logger.logInfo(assertStr)
      }
    }
    assert(cond, assertStr)
  }
}
