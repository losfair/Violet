module Orange.Frontend.DecodeDep where

import Clash.Prelude
import Orange.Types.DecodeDep
import qualified Orange.Types.Fetch as FetchT

decode :: FetchT.Inst -> (Activation, RegLayout, Stall)
decode inst = case slice d6 d0 inst of
    0b0110111 -> (intActivation, Rd, StallNone) -- lui
    0b0010111 -> (intActivation, Rd, StallNone) -- auipc
    0b1101111 -> (jalActivation, Rd, StallNone) -- jal
    0b1100111 -> (jalActivation, RdRs1, StallNone) -- jalr
    0b1100011 -> (jActivation, Rs1Rs2, StallNone) -- beq/bne/blt/bge/bltu/bgeu
    0b0000011 -> (memActivation, RdRs1, StallMemory) -- load
    0b0100011 -> (memActivation, Rs1Rs2, StallNone) -- store
    0b0010011 -> (intActivation, RdRs1, StallNone) -- ALU with imm
    0b0110011 -> case slice d31 d25 inst of
        0b0000001 -> case slice d14 d14 inst of
            0b0 -> (intActivation, RdRs1Rs2, StallNone) -- mul
            0b1 -> (ctrlActivation, RdRs1Rs2, StallControl) -- div
        _ -> (intActivation, RdRs1Rs2, StallNone)
    0b0001111 -> (ctrlActivation, RdRs1, StallControl) -- fence
    0b1110011 -> (ctrlActivation, NoReg, StallControl) -- ecall/ebreak
    _ -> (excActivation, NoReg, StallNone)

dep :: (FetchT.Inst, Activation, RegLayout) -> (FetchT.Inst, Activation, RegLayout) -> Concurrency
dep (inst1, act1, layout1) (inst2, act2, layout2) = if anyHazard then NoConcurrentIssue else CanConcurrentIssue
    where
        branchConflict = actBranch act1 && actBranch act2 -- we cannot issue two branches concurrently
        memConflict = actMem act2 -- mem can only issue on completion pipe 1
        ctrlConflict = actCtrl act1 || actCtrl act2 -- control cannot issue concurrently with any other instructions
        exceptionConflict = actException act1 || actException act2 -- issue decoder exceptions in order
        structuralHazard = branchConflict || memConflict || ctrlConflict || exceptionConflict

        inst1Rs1 = slice d19 d15 inst1
        inst1Rs2 = slice d24 d20 inst1
        inst1Rd = slice d11 d7 inst1

        inst2Rs1 = slice d19 d15 inst2
        inst2Rs2 = slice d24 d20 inst2
        inst2Rd = slice d11 d7 inst2

        inst1WritesRd = layout1 == Rd || layout1 == RdRs1 || layout1 == RdRs1Rs2
        inst2ReadsRs1 = layout2 == RdRs1 || layout2 == RdRs1Rs2 || layout2 == Rs1Rs2
        inst2ReadsRs2 = layout2 == RdRs1Rs2 || layout2 == Rs1Rs2
        regConflictCase1 = inst1WritesRd && inst2ReadsRs1 && inst1Rd == inst2Rs1 && inst1Rd /= 0
        regConflictCase2 = inst1WritesRd && inst2ReadsRs2 && inst1Rd == inst2Rs2 && inst1Rd /= 0
        regHazard = regConflictCase1 || regConflictCase2

        anyHazard = structuralHazard || regHazard

decodeDep' :: (DecodeDepBundle, DecodeDepBundle, (FetchT.Inst, Activation, RegLayout))
           -> ((FetchT.PC, FetchT.Inst), (FetchT.PC, FetchT.Inst))
           -> ((DecodeDepBundle, DecodeDepBundle, (FetchT.Inst, Activation, RegLayout)), (DecodeDepBundle, DecodeDepBundle))
decodeDep' (bundle1, bundle2, lastDecoded) ((pc1, inst1), (pc2, inst2)) = ((bundle1', bundle2', (inst2, activation2, regLayout2)), (bundle1, bundle2))
    where
        (activation1, regLayout1, stall1) = decode inst1
        (activation2, regLayout2, stall2) = decode inst2
        concurrency1 = dep lastDecoded (inst1, activation1, regLayout1)
        concurrency2 = dep (inst1, activation1, regLayout1) (inst2, activation2, regLayout2)
        bundle1' = (pc1, inst1, activation1, regLayout1, concurrency1, stall1)
        bundle2' = (pc2, inst2, activation2, regLayout2, concurrency2, stall2)

decodeDep :: HiddenClockResetEnable dom
          => Signal dom ((FetchT.PC, FetchT.Inst), (FetchT.PC, FetchT.Inst))
          -> Signal dom (DecodeDepBundle, DecodeDepBundle)
decodeDep = mealy decodeDep' (emptyDecodeBundle, emptyDecodeBundle, (FetchT.nopInst, emptyActivation, NoReg))

emptyDecodeBundle :: DecodeDepBundle
emptyDecodeBundle = (0, FetchT.nopInst, emptyActivation, NoReg, CanConcurrentIssue, StallNone)

emptyActivation :: Activation
emptyActivation = Activation {
    actInt = False,
    actBranch = False,
    actMem = False,
    actCtrl = False,
    actException = False
}

intActivation :: Activation
intActivation = Activation {
    actInt = True,
    actBranch = False,
    actMem = False,
    actCtrl = False,
    actException = False
}

jalActivation :: Activation
jalActivation = Activation {
    actInt = True,
    actBranch = True,
    actMem = False,
    actCtrl = False,
    actException = False
}

jActivation :: Activation
jActivation = Activation {
    actInt = False,
    actBranch = True,
    actMem = False,
    actCtrl = False,
    actException = False
}

memActivation :: Activation
memActivation = Activation {
    actInt = False,
    actBranch = False,
    actMem = True,
    actCtrl = False,
    actException = False
}

ctrlActivation :: Activation
ctrlActivation = Activation {
    actInt = False,
    actBranch = False,
    actMem = False,
    actCtrl = True,
    actException = False
}

excActivation :: Activation
excActivation = Activation {
    actInt = False,
    actBranch = False,
    actMem = False,
    actCtrl = False,
    actException = True
}
