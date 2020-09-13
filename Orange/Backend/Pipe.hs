module Orange.Backend.Pipe where

import Clash.Prelude
import Orange.Types.Pipe

genPipe :: HiddenClockResetEnable dom
     => KnownNat n
     => NFDataX a
     => a
     -> Vec n (Signal dom a)
     -> Vec n (Signal dom a)
genPipe initial = map (register initial)

completionPipe :: HiddenClockResetEnable dom
               => Vec PipeSize (Signal dom Commit)
               -> Vec PipeSize (Signal dom Commit)
completionPipe = genPipe Bubble

recoveryPipe :: HiddenClockResetEnable dom
             => Vec PipeSize (Signal dom Recovery)
             -> Vec PipeSize (Signal dom Recovery)
recoveryPipe = genPipe NotRecovery
