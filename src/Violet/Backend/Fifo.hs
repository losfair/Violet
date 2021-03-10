module Violet.Backend.Fifo where

import Clash.Prelude
import Violet.Types.Fifo
import qualified Violet.Types.Fetch as FetchT
import qualified Debug.Trace
import qualified Prelude
import qualified Violet.Backend.DualPort as DualPort

fifo' :: (Unsigned FifoBits, Unsigned FifoBits, FifoPushCap)
      -> (FifoItem, FifoItem, FifoPopReq)
      -> ((Unsigned FifoBits, Unsigned FifoBits, FifoPushCap), (Maybe (Unsigned FifoBits), Maybe (Unsigned FifoBits), Maybe (Unsigned FifoBits), Maybe (Unsigned FifoBits), FifoPushCap))
fifo' (rptr, wptr, pushCap) (witem1, witem2, popReq) = ((rptr', wptr', pushCap'), (ritem1P, ritem2P, witem1P, witem2P, pushCap))
    where
        flush = mkFlush witem1
        wptr_ = case flush of
            True -> 0
            False -> wptr
        wptr' = case (witem1, witem2) of
                    (Bubble, Bubble) -> wptr_
                    (_, Bubble) -> wptr_ + 1
                    _ -> wptr_ + 2
        rptr' = case flush of
            True -> 0
            False -> case popReq of
                        PopNothing -> rptr
                        PopOne -> rptr + 1
                        PopTwo -> rptr + 2
        ritem1P = if rptr' == wptr_ then Nothing else Just rptr'
        ritem2P = if rptr' == wptr_ || rptr' + 1 == wptr_ then Nothing else Just $ rptr' + 1
        willFull = wptr' + 1 == rptr' || wptr' + 2 == rptr' || wptr' + 3 == rptr' || wptr' + 4 == rptr'
        pushCap' = if willFull then WillFull else CanPush
        (witem1P, witem2P) = case witem1 of
            Bubble -> (Nothing, Nothing)
            _ -> case witem2 of
                Bubble -> (Just wptr_, Nothing)
                _ -> (Just wptr_, Just $ wptr_ + 1)

fifo :: HiddenClockResetEnable dom
     => Signal dom (FifoItem, FifoItem, FifoPopReq)
     -> Signal dom (FifoItem, FifoItem, FifoPushCap)
fifo inputs = bundle (ram1, ram2, pushCap)
    where
        (witem1, witem2, _) = unbundle inputs
        wp1 = fmap mkWp $ bundle (witem1, witem1P)
        wp2 = fmap mkWp $ bundle (witem2, witem2P)
        ram1 = fmap unwrapDpramResult $ bundle (ritem1P, DualPort.dpram (fmap forceUnwrap ritem1P) wp1 wp2)
        ram2 = fmap unwrapDpramResult $ bundle (ritem2P, DualPort.dpram (fmap forceUnwrap ritem2P) wp1 wp2)
        (ritem1P, ritem2P, witem1P, witem2P, pushCap) = unbundle $ (mealy fifo' (0, 0, CanPush)) inputs

mkFlush :: FifoItem -> Bool
mkFlush (Item (_, _, md)) = FetchT.exceptionResolved md
mkFlush Bubble = False

mkWp :: (FifoItem, Maybe (Unsigned FifoBits)) -> Maybe (Unsigned FifoBits, FifoItem)
mkWp (x, (Just y)) = Just (y, x)
mkWp (_, Nothing) = Nothing

unwrapDpramResult :: (Maybe (Unsigned FifoBits), FifoItem) -> FifoItem
unwrapDpramResult (Nothing, _) = Bubble
unwrapDpramResult (_, x) = x

forceUnwrap :: Maybe (Unsigned FifoBits) -> (Unsigned FifoBits)
forceUnwrap (Just x) = x
forceUnwrap Nothing = undefined