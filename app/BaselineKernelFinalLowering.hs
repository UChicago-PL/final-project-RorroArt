module BaselineKernelFinalLowering
  ( lowerBaselineKernelFinalIR
  ) where

import qualified Data.Map.Strict as M
import BaselineKernel (BaselineDebugKey)
import qualified BaselineKernel as Direct
import qualified BaselineKernelFinalIR as Final
import FinalIR (Id, Kernel)
import FinalLowering (lowerKernelWithDebug)
import ISA (Bundle)

lowerBaselineKernelFinalIR
  :: Kernel Final.BaselineDebugKeyRef
  -> Either String [Bundle BaselineDebugKey]
lowerBaselineKernelFinalIR = lowerKernelWithDebug resolveDebugKey

resolveDebugKey :: M.Map Id Int -> Final.BaselineDebugKeyRef -> Either String BaselineDebugKey
resolveDebugKey env key =
  case key of
    Final.KeyIdx roundRef batchRef ->
      Direct.KeyIdx <$> resolveRef env roundRef <*> resolveRef env batchRef
    Final.KeyVal roundRef batchRef ->
      Direct.KeyVal <$> resolveRef env roundRef <*> resolveRef env batchRef
    Final.KeyNodeVal roundRef batchRef ->
      Direct.KeyNodeVal <$> resolveRef env roundRef <*> resolveRef env batchRef
    Final.KeyHashStage roundRef batchRef stage ->
      Direct.KeyHashStage <$> resolveRef env roundRef <*> resolveRef env batchRef <*> pure stage
    Final.KeyHashedVal roundRef batchRef ->
      Direct.KeyHashedVal <$> resolveRef env roundRef <*> resolveRef env batchRef
    Final.KeyNextIdx roundRef batchRef ->
      Direct.KeyNextIdx <$> resolveRef env roundRef <*> resolveRef env batchRef
    Final.KeyWrappedIdx roundRef batchRef ->
      Direct.KeyWrappedIdx <$> resolveRef env roundRef <*> resolveRef env batchRef

resolveRef :: M.Map Id Int -> Final.BaselineDebugRef -> Either String Int
resolveRef _ (Final.RefConst n) = Right n
resolveRef env (Final.RefIv i) =
  case M.lookup i env of
    Just n -> Right n
    Nothing -> Left ("Debug key references non-constant id: " ++ show i)
