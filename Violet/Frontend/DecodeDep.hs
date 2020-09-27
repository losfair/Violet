module Violet.Frontend.DecodeDep where

import Clash.Prelude
import Violet.Types.DecodeDep
import qualified Violet.Types.Fetch as FetchT
import qualified Violet.Types.Fifo as FifoT

decode :: FetchT.Inst -> (Activation, RegLayout, Stall)
decode inst = case slice d6 d0 inst of
    0b0110111 -> (intActivation, layoutRd, StallNone) -- lui
    0b0010111 -> (intActivation, layoutRd, StallNone) -- auipc
    0b1101111 -> (jalActivation, layoutRd, StallNone) -- jal
    0b1100111 -> (jalActivation, layoutRdRs1, StallNone) -- jalr
    0b1100011 -> (jActivation, layoutRs1Rs2, StallNone) -- beq/bne/blt/bge/bltu/bgeu
    0b0000011 -> (loadActivation, layoutRdRs1, StallMemory) -- load
    0b0100011 -> (storeActivation, layoutRs1Rs2, StallNone) -- store
    0b0010011 -> (intActivation, layoutRdRs1, StallNone) -- ALU with imm
    0b0110011 -> case slice d31 d25 inst of
        0b0000001 -> case slice d14 d14 inst of
            0b0 -> (ctrlActivation, layoutRdRs1Rs2, StallNone) -- mul
            0b1 -> (ctrlActivation, layoutRdRs1Rs2, StallControl) -- div
        _ -> (intActivation, layoutRdRs1Rs2, StallNone)
    0b0001111 -> (ctrlActivation, layoutRdRs1, StallControl) -- fence
    0b1110011 -> (ctrlActivation, layoutNoReg, StallControl) -- ecall/ebreak
    _ -> (excActivation, layoutNoReg, StallNone)

dep :: (FetchT.Inst, Activation, RegLayout) -> (FetchT.Inst, Activation, RegLayout) -> Concurrency
dep (inst1, act1, layout1) (inst2, act2, layout2) = if anyHazard then NoConcurrentIssue else CanConcurrentIssue
    where
        branchConflict = actBranch act2 -- port 1 only
        memConflict = actLoad act2 || actStore act2 -- port 1 only
        ctrlConflict = actCtrl act1 || actCtrl act2
        exceptionConflict = actException act1 || actException act2
        structuralHazard = branchConflict || memConflict || ctrlConflict || exceptionConflict

        inst1Rs1 = slice d19 d15 inst1
        inst1Rs2 = slice d24 d20 inst1
        inst1Rd = slice d11 d7 inst1

        inst2Rs1 = slice d19 d15 inst2
        inst2Rs2 = slice d24 d20 inst2
        inst2Rd = slice d11 d7 inst2

        inst1WritesRd = hasRd layout1
        inst2ReadsRs1 = hasRs1 layout2
        inst2ReadsRs2 = hasRs2 layout2
        regConflictCase1 = inst1WritesRd && inst2ReadsRs1 && inst1Rd == inst2Rs1 && inst1Rd /= 0
        regConflictCase2 = inst1WritesRd && inst2ReadsRs2 && inst1Rd == inst2Rs2 && inst1Rd /= 0
        regHazard = regConflictCase1 || regConflictCase2

        anyHazard = structuralHazard || regHazard

decodeDep' :: (FifoT.FifoItem, FifoT.FifoItem, (FetchT.Inst, Activation, RegLayout))
           -> ((FetchT.PC, FetchT.Inst, FetchT.Metadata), (FetchT.PC, FetchT.Inst, FetchT.Metadata), FifoT.FifoPushCap)
           -> ((FifoT.FifoItem, FifoT.FifoItem, (FetchT.Inst, Activation, RegLayout)), (FifoT.FifoItem, FifoT.FifoItem))
decodeDep' (bundle1, bundle2, lastDecoded) ((pc1, inst1, md1), (pc2, inst2, md2), pushCap) = ((bundle1', bundle2', (inst2, activation2, regLayout2)), (bundle1, bundle2))
    where
        (activation1, regLayout1, stall1) = decode inst1
        (activation2, regLayout2, stall2) = decode inst2
        concurrency1 = dep lastDecoded (inst1, activation1, regLayout1)
        concurrency2 = dep (inst1, activation1, regLayout1) (inst2, activation2, regLayout2)
        bundle1' = if pushCap == FifoT.CanPush && FetchT.isValidInst md1 then FifoT.Item (pc1, inst1, md1, activation1, regLayout1, concurrency1, stall1) else FifoT.Bubble
        bundle2' = if pushCap == FifoT.CanPush && FetchT.isValidInst md2 then FifoT.Item (pc2, inst2, md2, activation2, regLayout2, concurrency2, stall2) else FifoT.Bubble

decodeDep :: HiddenClockResetEnable dom
          => Signal dom ((FetchT.PC, FetchT.Inst, FetchT.Metadata), (FetchT.PC, FetchT.Inst, FetchT.Metadata), FifoT.FifoPushCap)
          -> Signal dom (FifoT.FifoItem, FifoT.FifoItem)
decodeDep = mealy decodeDep' (FifoT.Bubble, FifoT.Bubble, (FetchT.nopInst, emptyActivation, layoutNoReg))

emptyActivation :: Activation
emptyActivation = Activation {
    actInt = False,
    actBranch = False,
    actLoad = False,
    actStore = False,
    actCtrl = False,
    actException = False
}

intActivation :: Activation
intActivation = Activation {
    actInt = True,
    actBranch = False,
    actLoad = False,
    actStore = False,
    actCtrl = False,
    actException = False
}

jalActivation :: Activation
jalActivation = Activation {
    actInt = True,
    actBranch = True,
    actLoad = False,
    actStore = False,
    actCtrl = False,
    actException = False
}

jActivation :: Activation
jActivation = Activation {
    actInt = False,
    actBranch = True,
    actLoad = False,
    actStore = False,
    actCtrl = False,
    actException = False
}

loadActivation :: Activation
loadActivation = Activation {
    actInt = False,
    actBranch = False,
    actLoad = True,
    actStore = False,
    actCtrl = False,
    actException = False
}

storeActivation :: Activation
storeActivation = Activation {
    actInt = False,
    actBranch = False,
    actLoad = False,
    actStore = True,
    actCtrl = False,
    actException = False
}

ctrlActivation :: Activation
ctrlActivation = Activation {
    actInt = False,
    actBranch = False,
    actLoad = False,
    actStore = False,
    actCtrl = True,
    actException = False
}

excActivation :: Activation
excActivation = Activation {
    actInt = False,
    actBranch = False,
    actLoad = False,
    actStore = False,
    actCtrl = False,
    actException = True
}
