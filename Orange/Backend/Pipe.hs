module Orange.Backend.Pipe where

import Clash.Prelude
import Orange.Types.Pipe

genPipe :: HiddenClockResetEnable dom
     => KnownNat n
     => NFDataX a
     => a
     -> Signal dom (Vec n a)
     -> Signal dom (Vec n a)
genPipe initial = register (repeat initial)

completionPipe :: HiddenClockResetEnable dom
               => Signal dom (Vec PipeSize Commit)
               -> Signal dom (Vec PipeSize Commit)
completionPipe = genPipe Bubble

recoveryPipe :: HiddenClockResetEnable dom
             => Signal dom (Vec PipeSize Recovery)
             -> Signal dom (Vec PipeSize Recovery)
recoveryPipe = genPipe NotRecovery
