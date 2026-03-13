module Bundle
  ( bundleProgram
  ) where

import Data.List (foldl')
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as M
import Data.Set (Set)
import qualified Data.Set as S

import ISA (SlotLimits(..), defaultSlotLimits, AluSlot, ValuSlot, LoadSlot, StoreSlot, FlowSlot)
import qualified ISA
import Machine

-- | Greedy in-order packetizer.  Packs consecutive ops into the same
-- bundle when legal w.r.t. slot limits, RAW/WAW hazards, and memory.
bundleProgram :: [MachineOp] -> [ISA.Bundle ()]
bundleProgram ops = bsDone (flushBundle (foldl' step emptyState ops))

data BState = BState
  { bsAlu    :: ![AluSlot]
  , bsValu   :: ![ValuSlot]
  , bsLoad   :: ![LoadSlot]
  , bsStore  :: ![StoreSlot]
  , bsFlow   :: ![FlowSlot]
  , bsWrites :: !(Set Int)
  , bsStores :: !(Map String [MemRange])
  , bsDone   :: ![ISA.Bundle ()]
  , bsEmpty  :: !Bool
  }

emptyState :: BState
emptyState = BState [] [] [] [] [] S.empty M.empty [] True

flushBundle :: BState -> BState
flushBundle st
  | bsEmpty st = st
  | otherwise  =
      let b = ISA.Bundle
            { ISA.aluSlots   = reverse (bsAlu st)
            , ISA.valuSlots  = reverse (bsValu st)
            , ISA.loadSlots  = reverse (bsLoad st)
            , ISA.storeSlots = reverse (bsStore st)
            , ISA.flowSlots  = reverse (bsFlow st)
            , ISA.debugSlots = []
            }
      in emptyState { bsDone = bsDone st ++ [b] }

step :: BState -> MachineOp -> BState
step st op =
  if canPack op st
     then addOp st op
     else addOp (flushBundle st) op

addOp :: BState -> MachineOp -> BState
addOp st op =
  let st1 = case moSlot op of
              MSlotAlu s   -> st { bsAlu   = s : bsAlu st }
              MSlotValu s  -> st { bsValu  = s : bsValu st }
              MSlotLoad s  -> st { bsLoad  = s : bsLoad st }
              MSlotStore s -> st { bsStore = s : bsStore st }
              MSlotFlow s  -> st { bsFlow  = s : bsFlow st }
      writes1 = S.union (bsWrites st1) (S.fromList (moWrites op))
      stores1 = foldl' addStore (bsStores st1) (moMem op)
  in st1 { bsWrites = writes1, bsStores = stores1, bsEmpty = False }
  where
    addStore m ma =
      if maKind ma == "store"
         then M.insertWith (++) (maRegion ma) [maRange ma] m
         else m

canPack :: MachineOp -> BState -> Bool
canPack op st =
     engineHasRoom
  && noRaw
  && noWaw
  && noMemHazard
  where
    limits = defaultSlotLimits

    engineHasRoom =
      case moSlot op of
        MSlotAlu _   -> length (bsAlu st) < slAlu limits
        MSlotValu _  -> length (bsValu st) < slValu limits
        MSlotLoad _  -> length (bsLoad st) < slLoad limits
        MSlotStore _ -> length (bsStore st) < slStore limits
        MSlotFlow _  -> length (bsFlow st) < slFlow limits

    noRaw =
      let w = bsWrites st
      in all (\r -> not (S.member r w)) (moReads op)

    noWaw =
      let w = bsWrites st
      in all (\r -> not (S.member r w)) (moWrites op)

    noMemHazard = all okAccess (moMem op)

    okAccess ma =
      case M.lookup (maRegion ma) (bsStores st) of
        Nothing -> True
        Just rs -> all (\r -> not (overlap (maRange ma) r)) rs

overlap :: MemRange -> MemRange -> Bool
overlap Unknown _ = True
overlap _ Unknown = True
overlap (Range a0 a1) (Range b0 b1) = not (a1 < b0 || b1 < a0)
