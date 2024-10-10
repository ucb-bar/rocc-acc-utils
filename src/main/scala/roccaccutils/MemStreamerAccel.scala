// See LICENSE for license details

package roccaccutils

import scala.collection.immutable.{ListMap}

import chisel3._

import org.chipsalliance.cde.config.{Parameters, Field}

import freechips.rocketchip.tile._
import freechips.rocketchip.rocket.{TLBConfig}
import freechips.rocketchip.diplomacy._
import freechips.rocketchip.rocket.constants.MemoryOpConstants
import freechips.rocketchip.tilelink._
import freechips.rocketchip.subsystem.{SystemBusKey}
import freechips.rocketchip.subsystem.{CacheBlockBytes}
import freechips.rocketchip.resources.{DiplomacyUtils}

import testchipip.soc.{BankedScratchpadParams}

import roccaccutils.logger._

abstract class MemStreamerAccel(opcodes: OpcodeSet)(implicit p: Parameters)
  extends LazyRoCC(opcodes=opcodes, nPTWPorts=2)
  with HasL2MemHelperParams {

  // --------------------------
  // MUST BE DEFINED BY CHILD
  // --------------------------

  val tlbConfig: TLBConfig
  val xbarBetweenMem: Boolean
  val logger: Logger
  val spad: Option[BankedScratchpadParams]

  // --------------------------

  implicit val hp: L2MemHelperParams = L2MemHelperParams(p(SystemBusKey).beatBytes * 8)

  val roccTLNode = if (xbarBetweenMem) atlNode else tlNode

  val xbar = LazyModule(new TLXbar)

  val l2_memloader =     LazyModule(new L2MemHelper(tlbConfig, printInfo="[memloader]", numOutstandingReqs=32, logger=logger))
  xbar.node := TLBuffer.chainNode(1) := l2_memloader.masterNode

  val l2_memwriter =     LazyModule(new L2MemHelper(tlbConfig, printInfo="[memwriter]", numOutstandingReqs=32, logger=logger))
  xbar.node := TLBuffer.chainNode(1) := l2_memwriter.masterNode

  val busBeatBytes = BUS_SZ_BYTES
  val intNode = spad match {
    case Some(BankedScratchpadParams(base, size, _, banks, subbanks, _, _, _, _, _)) => {
      val spad_xbar = LazyModule(new TLXbar).suggestName("streamer_spad_xbar")

      // input multi-banked globally visible scratchpad
      val bankStripe = p(CacheBlockBytes)*subbanks
      val mask = (banks-1)*bankStripe
      val device = new MemoryDevice {
        override def describe(resources: ResourceBindings): Description = {
          Description(describeName("memory", resources), ListMap(
            "reg"         -> resources.map.filterKeys(DiplomacyUtils.regFilter).flatMap(_._2).map(_.value).toList,
            "device_type" -> Seq(ResourceString("memory")),
            "status"      -> Seq(ResourceString("okay"))
          ))
        }
      }
      (0 until banks).map { b =>
        val bank = LazyModule(new testchipip.soc.ScratchpadBankNonClockDiplomatic(
            subbanks,
            AddressSet(base + bankStripe * b, size - 1 - mask),
            busBeatBytes,
            device,
            BufferParams.default))
        bank.xbar := TLBuffer(BufferParams.default) := spad_xbar.node
      }

      // let outer memory come to scratchpad
      spad_xbar.node := TLBuffer() := TLWidthWidget(busBeatBytes) := stlNode
      // allow mem. reqs. to go to spad w/o going out to sbus
      spad_xbar.node := TLWidthWidget(busBeatBytes) := xbar.node

      TLFilter(TLFilter.mSubtract(Seq(AddressSet(base, size-1))))
    }
    case None => TLIdentityNode()
  }

  // let acc mem. reqs come be sent out (filter out all from spad)
  roccTLNode := intNode := TLWidthWidget(busBeatBytes) := xbar.node
}

abstract class MemStreamerAccelImp(outer: MemStreamerAccel)(implicit p: Parameters)
  extends LazyRoCCModuleImp(outer) with MemoryOpConstants {

  // --------------------------
  // MUST BE DEFINED BY CHILD
  // --------------------------

  val queueDepth: Int
  val cmd_router: StreamingCommandRouter
  val streamer: MemStreamer

  // --------------------------

  implicit val hp: L2MemHelperParams = outer.hp

  io.mem.req.valid := false.B
  io.mem.s1_kill := false.B
  io.mem.s2_kill := false.B
  io.mem.keep_clock_enabled := true.B
  io.interrupt := false.B
  io.busy := false.B

  val memloader = Module(new MemLoader(memLoaderQueDepth=queueDepth, logger=outer.logger))
  outer.l2_memloader.module.io.userif <> memloader.io.l2helperUser
  memloader.io.src_info <> cmd_router.io.src_info

  val memwriter = Module(new MemWriter32(cmd_que_depth=queueDepth, logger=outer.logger))
  outer.l2_memwriter.module.io.userif <> memwriter.io.l2io

  outer.l2_memloader.module.io.sfence <> cmd_router.io.sfence_out
  outer.l2_memloader.module.io.status.valid := cmd_router.io.dmem_status_out.valid
  outer.l2_memloader.module.io.status.bits := cmd_router.io.dmem_status_out.bits.status
  io.ptw(0) <> outer.l2_memloader.module.io.ptw

  outer.l2_memwriter.module.io.sfence <> cmd_router.io.sfence_out
  outer.l2_memwriter.module.io.status.valid := cmd_router.io.dmem_status_out.valid
  outer.l2_memwriter.module.io.status.bits := cmd_router.io.dmem_status_out.bits.status
  io.ptw(1) <> outer.l2_memwriter.module.io.ptw

  cmd_router.io.rocc_in <> io.cmd
  io.resp <> cmd_router.io.rocc_out

  streamer.io.mem_stream <> memloader.io.consumer
  memwriter.io.memwrites_in <> streamer.io.memwrites_in
  memwriter.io.decompress_dest_info <> cmd_router.io.dest_info
  cmd_router.io.bufs_completed := memwriter.io.bufs_completed
  cmd_router.io.no_writes_inflight := memwriter.io.no_writes_inflight
}
