module BaselineDebugPolicy
  ( baselineDebugPolicy
  ) where

import BaselineKernelFinalIR (BaselineDebugKeyRef(..), BaselineDebugRef(..))
import BatchVectorizeFinalIR (DebugPolicy(..))
import FinalIR (Id)

baselineDebugPolicy :: DebugPolicy BaselineDebugKeyRef
baselineDebugPolicy =
  DebugPolicy
    { rewriteScalarDebugKey = \oldIv newIv key ->
        Right (substKey oldIv newIv key)
    , expandVectorDebugKey = \oldIv laneIvs key ->
        Right (map (\laneIv -> substKey oldIv laneIv key) laneIvs)
    , rewriteExistingVectorDebugKeys = \oldIv laneIvs keys ->
        if length laneIvs == length keys
          then Right (zipWith (\laneIv key -> substKey oldIv laneIv key) laneIvs keys)
          else Left "batchvec: EDebugCompareV key count mismatch for baseline debug policy"
    }

substKey :: Id -> Id -> BaselineDebugKeyRef -> BaselineDebugKeyRef
substKey oldIv newIv key = case key of
  KeyIdx r b -> KeyIdx (substRef r) (substRef b)
  KeyVal r b -> KeyVal (substRef r) (substRef b)
  KeyNodeVal r b -> KeyNodeVal (substRef r) (substRef b)
  KeyHashStage r b s -> KeyHashStage (substRef r) (substRef b) s
  KeyHashedVal r b -> KeyHashedVal (substRef r) (substRef b)
  KeyNextIdx r b -> KeyNextIdx (substRef r) (substRef b)
  KeyWrappedIdx r b -> KeyWrappedIdx (substRef r) (substRef b)
  where
    substRef ref = case ref of
      RefConst n -> RefConst n
      RefIv i | i == oldIv -> RefIv newIv
      RefIv i -> RefIv i
