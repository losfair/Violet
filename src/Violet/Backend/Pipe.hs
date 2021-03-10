module Violet.Backend.Pipe where

import Clash.Prelude
import Violet.Types.Pipe

recoverableRegister :: HiddenClockResetEnable dom
                    => NFDataX a
                    => a
                    -> Signal dom Recovery
                    -> Signal dom a
                    -> Signal dom a
recoverableRegister initial recovery next = r
     where
          r = register initial (fmap nextS $ bundle (recovery, next))
          nextS (recovery, next) = case recovery of
               IsRecovery -> initial
               NotRecovery -> next

completionPipe :: HiddenClockResetEnable dom
               => Signal dom Recovery
               -> Vec PipeSize (Signal dom Commit)
               -> Vec PipeSize (Signal dom Commit)
completionPipe recovery = map (recoverableRegister Bubble recovery)

recoveryPipe :: HiddenClockResetEnable dom
             => Vec PipeSize (Signal dom Recovery)
             -> Vec PipeSize (Signal dom Recovery)
recoveryPipe = map (register NotRecovery)
