module DCE
  ( eliminateDeadCode
  ) where

import Data.Set (Set)
import qualified Data.Set as S

import FinalIR

-- | Extract referenced Ids from an expression.
exprIds :: Expr -> Set Id
exprIds (Var i)   = S.singleton i
exprIds (Const _) = S.empty

-- | Collect all Ids referenced by a Rhs.
rhsIds :: Rhs -> Set Id
rhsIds (RConst _)          = S.empty
rhsIds (RBin _ a b)        = exprIds a <> exprIds b
rhsIds (RMulAdd a b c)     = exprIds a <> exprIds b <> exprIds c
rhsIds (RSelect a b c)     = exprIds a <> exprIds b <> exprIds c
rhsIds (RLoad _ _ e)       = exprIds e
rhsIds (RVBroadcast _ e)   = exprIds e
rhsIds (RReduce _ e)       = exprIds e

-- | Backward pass: compute the set of used Ids over a list of statements.
computeUsed :: [Stmt] -> Set Id
computeUsed = foldr step S.empty
  where
    step (Let x _ rhs) used
      | x `S.member` used = used <> rhsIds rhs
      | otherwise          = used
    step (Store _ _ ix val) used =
      used <> exprIds ix <> exprIds val
    step (For iv _ _ _ carries body) used =
      let bodyUsed   = computeUsed body
          carryIds   = foldMap (exprIds . snd) carries
      in  used <> S.insert iv bodyUsed <> carryIds

-- | Forward pass: remove dead Let bindings, recurse into For bodies.
filterDead :: Set Id -> [Stmt] -> [Stmt]
filterDead used = concatMap go
  where
    go s@(Let x _ _)
      | x `S.member` used = [s]
      | otherwise          = []
    go s@(Store {})        = [s]
    go (For iv start end step carries body) =
      let body' = filterDead (computeUsed body) body
      in  [For iv start end step carries body']

-- | Eliminate dead code from a Function.
eliminateDeadCode :: Function -> Function
eliminateDeadCode (Function body) =
  let used = computeUsed body
  in  Function (filterDead used body)
