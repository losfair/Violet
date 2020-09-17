module Orange.Backend.Fifo where

import Clash.Prelude
import Orange.Types.Fifo
import qualified Debug.Trace
import qualified Prelude

fifo' :: (Vec (2^FifoBits) FifoItem, Unsigned FifoBits, Unsigned FifoBits, FifoPushCap)
      -> (FifoItem, FifoItem, FifoPopReq)
      -> ((Vec (2^FifoBits) FifoItem, Unsigned FifoBits, Unsigned FifoBits, FifoPushCap), (FifoItem, FifoItem, FifoPushCap))
fifo' (store, rptr, wptr, pushCap) (witem1, witem2, popReq) = ((store', rptr', wptr', pushCap'), (ritem1, ritem2, pushCap))
    where
        wptr' = case (witem1, witem2) of
                    (Bubble, Bubble) -> wptr
                    (_, Bubble) -> wptr + 1
                    _ -> wptr + 2
        rptr' = case popReq of
                    PopNothing -> rptr
                    PopOne -> rptr + 1
                    PopTwo -> rptr + 2
        ritem1 = if rptr' == wptr then Bubble else store !! rptr'
        ritem2 = if rptr' == wptr || rptr' + 1 == wptr then Bubble else store !! (rptr' + 1)
        willFull = wptr' + 1 == rptr' || wptr' + 2 == rptr' || wptr' + 3 == rptr' || wptr' + 4 == rptr'
        pushCap' = if willFull then WillFull else CanPush
        store' = case witem1 of
            Bubble -> store
            _ ->
                let storeT = replace wptr witem1 store in
                    case witem2 of
                        Bubble -> storeT
                        _ -> replace (wptr + 1) witem2 storeT

fifo :: HiddenClockResetEnable dom
     => Signal dom (FifoItem, FifoItem, FifoPopReq)
     -> Signal dom (FifoItem, FifoItem, FifoPushCap)
fifo = mealy fifo' (repeat Bubble, 0, 0, CanPush)
