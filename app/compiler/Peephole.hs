module Peephole
  ( combineMulAdd
  ) where

import ISA (AluOp(..))
import FinalIR

-- | Combine vector mul + add into RMulAdd when the intermediate is dead.
combineMulAdd :: Function -> Function
combineMulAdd (Function ss) = Function (goBlock ss)
  where
    goBlock [] = []
    goBlock (For iv s e step carries body : rest) =
      For iv s e step carries (goBlock body) : goBlock rest

    goBlock (Let t tyT (RBin Mul a b)
           : Let y tyY (RBin Add (Var t1) c)
           : rest)
      | t == t1
      , tyT == tyY
      , isVecTy tyT
      , not (usesId t rest)
      = Let y tyY (RMulAdd a b c) : goBlock rest

    goBlock (Let t tyT (RBin Mul a b)
           : Let y tyY (RBin Add c (Var t1))
           : rest)
      | t == t1
      , tyT == tyY
      , isVecTy tyT
      , not (usesId t rest)
      = Let y tyY (RMulAdd a b c) : goBlock rest

    goBlock (stmt : rest) = stmt : goBlock rest

isVecTy :: Ty -> Bool
isVecTy (Vec _ _) = True
isVecTy _         = False

usesId :: Id -> [Stmt] -> Bool
usesId x = any usesStmt
  where
    usesStmt stmt =
      case stmt of
        Let _ _ rhs -> usesRhs rhs
        Store _ _ ix v -> usesExpr ix || usesExpr v
        For _ _ _ _ _ body -> usesId x body

    usesRhs rhs =
      case rhs of
        RConst _ -> False
        RBin _ a b -> usesExpr a || usesExpr b
        RMulAdd a b c -> usesExpr a || usesExpr b || usesExpr c
        RSelect c a b -> usesExpr c || usesExpr a || usesExpr b
        RLoad _ _ ix -> usesExpr ix
        RVBroadcast _ e -> usesExpr e
        RReduce _ e -> usesExpr e

    usesExpr e =
      case e of
        Var y   -> y == x
        Const _ -> False
