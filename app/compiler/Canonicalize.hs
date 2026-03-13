module Canonicalize
  ( canonicalize
  ) where

import Data.Bits (xor, shiftL, shiftR, (.&.), (.|.))
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as M
import Data.Word (Word32)

import ISA (AluOp(..))
import FinalIR

-- | Constant propagation and folding.
canonicalize :: Function -> Function
canonicalize (Function ss) = Function (snd (goBlock M.empty ss))

goBlock :: Map Id Word32 -> [Stmt] -> (Map Id Word32, [Stmt])
goBlock env0 = foldl step (env0, [])
  where
    step (env, acc) stmt =
      case stmt of
        Let x ty rhs0 ->
          let rhs1 = simpRhs env rhs0
              env' = case rhsConst rhs1 of
                       Just c  -> M.insert x c env
                       Nothing -> M.delete x env
          in (env', acc ++ [Let x ty rhs1])

        Store k buf ix v ->
          (env, acc ++ [Store k buf (simpExpr env ix) (simpExpr env v)])

        For iv s e stepN body ->
          let (_envBody, body') = goBlock (M.delete iv env) body
          in (env, acc ++ [For iv s e stepN body'])

rhsConst :: Rhs -> Maybe Word32
rhsConst (RConst c) = Just c
rhsConst (RBin op (Const a) (Const b)) = Just (evalAluOp op a b)
rhsConst (RMulAdd (Const a) (Const b) (Const c)) = Just (a * b + c)
rhsConst (RSelect (Const c) a b) =
  case (if c /= 0 then a else b) of
    Const x -> Just x
    _       -> Nothing
rhsConst _ = Nothing

simpExpr :: Map Id Word32 -> Expr -> Expr
simpExpr env (Var x) = maybe (Var x) Const (M.lookup x env)
simpExpr _   c@(Const _) = c

simpRhs :: Map Id Word32 -> Rhs -> Rhs
simpRhs env rhs =
  case rhs of
    RConst c -> RConst c

    RBin op a b ->
      let a' = simpExpr env a
          b' = simpExpr env b
      in case (a', b') of
           (Const x, Const y) -> RConst (evalAluOp op x y)
           _ -> RBin op a' b'

    RMulAdd a b c ->
      let a' = simpExpr env a
          b' = simpExpr env b
          c' = simpExpr env c
      in case (a', b', c') of
           (Const x, Const y, Const z) -> RConst (x * y + z)
           _ -> RMulAdd a' b' c'

    RSelect c a b ->
      let c' = simpExpr env c
          a' = simpExpr env a
          b' = simpExpr env b
      in if a' == b'
            then RSelect (Const 1) a' b'
            else RSelect c' a' b'

    RLoad k buf ix -> RLoad k buf (simpExpr env ix)

    RVBroadcast w e -> RVBroadcast w (simpExpr env e)

evalAluOp :: AluOp -> Word32 -> Word32 -> Word32
evalAluOp op a b =
  case op of
    Add  -> a + b
    Sub  -> a - b
    Mul  -> a * b
    Div  -> if b == 0 then 0 else a `div` b
    Cdiv -> if b == 0 then 0 else (a + b - 1) `div` b
    Xor  -> a `xor` b
    And  -> a .&. b
    Or   -> a .|. b
    Shl  -> a `shiftL` fromIntegral b
    Shr  -> a `shiftR` fromIntegral b
    Mod  -> if b == 0 then 0 else a `mod` b
    Lt   -> if a < b then 1 else 0
    Eq_  -> if a == b then 1 else 0
