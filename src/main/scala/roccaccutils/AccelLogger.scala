// See LICENSE for license details

package roccaccutils

import chisel3._

import org.chipsalliance.cde.config.{Parameters}
import freechips.rocketchip.diplomacy.{ValName}

trait AccelLogger {
  // these must be defined
  def logInfoImplPrintWrapper(printf: chisel3.printf.Printf)(implicit p: Parameters): chisel3.printf.Printf
  def logCriticalImplPrintWrapper(printf: chisel3.printf.Printf)(implicit p: Parameters): chisel3.printf.Printf

  def trimValName()(implicit p: Parameters, valName: ValName): String = {
    val trimAmt = if (valName.name.startsWith("<local")) 6 else 1
    "<" + valName.name.substring(trimAmt, valName.name.length)
  }

  def logInfo(format: String, args: Bits*)(implicit p: Parameters, valName: ValName): Unit = {
    implicit val withMod = true
    logInfoImpl(format, args:_*)
  }

  def logCritical(format: String, args: Bits*)(implicit p: Parameters, valName: ValName): Unit = {
    implicit val withMod = true
    logCriticalImpl(format, args:_*)
  }

  def logInfoNoMod(format: String, args: Bits*)(implicit p: Parameters, valName: ValName): Unit = {
    implicit val withMod = false
    logInfoImpl(format, args:_*)
  }

  def logCriticalNoMod(format: String, args: Bits*)(implicit p: Parameters, valName: ValName): Unit = {
    implicit val withMod = false
    logCriticalImpl(format, args:_*)
  }

  def logInfoImpl(format: String, args: Bits*)(implicit p: Parameters, valName: ValName, withMod: Boolean): Unit = {
    val loginfo_cycles = RegInit(0.U(64.W))
    loginfo_cycles := loginfo_cycles + 1.U

    val prefix = s":INFO:%d:" + (if (withMod) s" ${trimValName()}:" else "") + " "

    logInfoImplPrintWrapper(printf(prefix, loginfo_cycles))
    logInfoImplPrintWrapper(printf(Printable.pack(format, args:_*)))
  }

  def logCriticalImpl(format: String, args: Bits*)(implicit p: Parameters, valName: ValName, withMod: Boolean): Unit = {
    val loginfo_cycles = RegInit(0.U(64.W))
    loginfo_cycles := loginfo_cycles + 1.U

    val prefix = s":CRIT:%d:" + (if (withMod) s" ${trimValName()}:" else "") + " "

    logCriticalImplPrintWrapper(printf(prefix, loginfo_cycles))
    logCriticalImplPrintWrapper(printf(Printable.pack(format, args:_*)))
  }
}

object DefaultAccelLogger extends AccelLogger {
  // just print info msgs
  def logInfoImplPrintWrapper(printf: chisel3.printf.Printf)(implicit p: Parameters): chisel3.printf.Printf = {
    printf
  }

  // just print critical msgs
  def logCriticalImplPrintWrapper(printf: chisel3.printf.Printf)(implicit p: Parameters): chisel3.printf.Printf = {
    printf
  }
}
