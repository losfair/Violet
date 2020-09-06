module Orange.Memory.ICache where

import Clash.Prelude
import Orange.Types
import Orange.Interlock
import Orange.TypeLevel.Nat
import qualified Orange.Config as Config
import qualified Orange.Memory.IBus as IBus
import qualified Orange.Memory.ICacheInterface as Iface

-- | tag | entry_index | line_internal_address
type Entries = $(natT (Config.instructionCacheEntries))
type EntryIndexBits = $(natT (floor $ logBase 2 $ fromIntegral Config.instructionCacheEntries))
type LineInternalAddressBits = $(natT (floor $ logBase 2 $ fromIntegral Config.instructionCacheLineWidth))
type TagBits = 32 - EntryIndexBits - LineInternalAddressBits
type LineWidthWords = $(natT (Config.instructionCacheLineWidth `div` 4))

data TagFetchOutput a = TagFetchOutput {
  tfAddress :: ByteAddress,
  tfAux :: a
} deriving (Generic, NFDataX)

data TagElement = NotPresent | PresentWith (BitVector TagBits)
  deriving (Generic, NFDataX)

data BackendFetchState = NotFetching | Fetching (Index LineWidthWords)
  deriving (Generic, NFDataX, Eq)

instructionCache :: HiddenClockResetEnable dom
  => Interlockable lkin
  => NFDataX a
  => Signal dom (Maybe (Iface.FetchIn a))
  -> Signal dom IBus.IBusIn
  -> Signal dom lkin
  -> Signal dom (Maybe (Iface.FetchOut a), IBus.IBusOut, Interlock lkin)
instructionCache reqIn ibusIn lockIn = bundle (fetchOutput, ibusOut, interlock)
  where
    req = fmap (\(reqIn, reqStaged, interlock) -> if isInterlocked interlock == Locked then reqStaged else reqIn) $ bundle (reqIn, reqStaged, interlock)
    reqStaged = register Nothing req
    ramReqAddr = fmap (\req -> case req of
      Just x -> Iface.inAddress x
      Nothing -> 0) req
    tagIndex = fmap (unpack . tagIndexFromAddress) ramReqAddr
    tagVal = blockRamPow2
      (replicate (SNat :: SNat Entries) NotPresent)
      tagIndex
      tagWritePort
    dataIndex = fmap (unpack . dataIndexFromAddress) ramReqAddr
    dataVal = blockRamPow2
      (replicate (SNat :: SNat (Entries * LineWidthWords)) (0 :: BitVector 32))
      dataIndex
      dataWritePort

    -- Backend fetching state
    backFetch = register NotFetching nextBackFetch

    -- Interlock
    interlock = fmap (\(lockIn, x, y) -> Interlock
        (if x /= NotFetching || y then Locked else Unlocked)
        lockIn
      ) $ bundle (lockIn, backFetch, backFetchRequest)

    -- Forward data path
    (tagOk, backFetchRequest) = unbundle $ fmap (\(tagVal, reqStaged, backFetch, lockIn) ->
        case (backFetch, isInterlocked lockIn, reqStaged) of
          (NotFetching, Unlocked, Just reqStaged) -> case (tagVal, tagFromAddress (Iface.inAddress reqStaged)) of
            (PresentWith tag, actualTag) | tag == actualTag -> (True, False)
            _ -> (False, True)
          _ -> (False, False)
      ) $ bundle (tagVal, reqStaged, backFetch, lockIn)

    -- Fetch engine
    (nextBackFetch, tagWritePort, dataWritePort, ibusOut) = unbundle $ fmap (\(backFetch, backFetchRequest, reqStaged', ibusIn) ->
      let
        reqStaged = case reqStaged' of
          Just x -> x
          Nothing -> undefined -- impossible
        lineBase = baseAddress $ Iface.inAddress reqStaged
        in
          case (backFetch, backFetchRequest) of
            (NotFetching, False) -> (NotFetching, Nothing, Nothing, IBus.emptyOut)
            (NotFetching, True) ->
              (Fetching 0, Nothing, Nothing, IBus.IBusOut { IBus.outValid = True, IBus.outAddress = lineBase })
            (Fetching i, _) ->
              if IBus.inReady ibusIn then
                let
                  addr = lineBase + (fromIntegral i) * 4
                  nextAddr = lineBase + (fromIntegral i) * 4
                  dw = (unpack $ dataIndexFromAddress addr, IBus.inData ibusIn)
                  in
                    if i == maxBound then
                      (
                        NotFetching,
                        Just (unpack $ tagIndexFromAddress lineBase, PresentWith $ tagFromAddress lineBase),
                        Just dw,
                        IBus.IBusOut { IBus.outValid = False, IBus.outAddress = 0 }
                      )
                    else
                      (
                        Fetching (i + 1),
                        Nothing,
                        Just dw,
                        IBus.IBusOut { IBus.outValid = True, IBus.outAddress = nextAddr }
                      )
              else
                (backFetch, Nothing, Nothing, IBus.emptyOut)
      ) $ bundle (backFetch, backFetchRequest, reqStaged, ibusIn)

    -- Mux things together
    fetchOutput = fmap (\(dataVal, tagOk, reqStaged, interlock) ->
        if isInterlocked interlock == Locked || tagOk == False then
          Nothing
        else
          Just $ Iface.FetchOut {
            Iface.outData = dataVal,
            Iface.outAux = case reqStaged of
              Just x -> Iface.inAux x
              Nothing -> undefined -- impossible
            }
      ) $ bundle (dataVal, tagOk, reqStaged, interlock)

tagFromAddress :: ByteAddress -> BitVector TagBits
tagFromAddress = slice d31 (SNat :: SNat (32 - TagBits))

tagIndexFromAddress :: ByteAddress -> BitVector EntryIndexBits
tagIndexFromAddress = slice (SNat :: SNat (LineInternalAddressBits + EntryIndexBits - 1)) (SNat :: SNat LineInternalAddressBits)

dataIndexFromAddress :: ByteAddress -> BitVector (LineInternalAddressBits + EntryIndexBits - 2)
dataIndexFromAddress = slice (SNat :: SNat (LineInternalAddressBits + EntryIndexBits - 1)) d2

baseAddress :: ByteAddress -> ByteAddress
baseAddress x = slice d31 (SNat :: SNat LineInternalAddressBits) x ++# 0