module Vectorize
  ( vectorizeBatch
  ) where

import Data.Map.Strict (Map)
import qualified Data.Map.Strict as M

import FinalIR

-- | Vectorize eligible batch loops into width-w SIMD with tail loop.
vectorizeBatch :: Int -> Function -> Function
vectorizeBatch w (Function ss) = Function (snd (goBlock M.empty ss))
  where
    goBlock :: Map Id Ty -> [Stmt] -> (Map Id Ty, [Stmt])
    goBlock env0 = foldl step (env0, [])

    step :: (Map Id Ty, [Stmt]) -> Stmt -> (Map Id Ty, [Stmt])
    step (env, acc) stmt =
      case stmt of
        Let x ty rhs ->
          (M.insert x ty env, acc ++ [Let x ty rhs])

        Store k buf ix v ->
          (env, acc ++ [Store k buf ix v])

        For iv s e stepN carries body ->
          let envIn = M.insert iv I32 env
          in if stepN == 1 && null carries && ivOnlyUsedAsInpIndex iv body
             then
               let tripCount = e - s
                   vecEnd = s + (tripCount `div` w) * w
                   (_envV, bodyV) = vectorizeBody w envIn iv body
                   loopV = For iv s vecEnd w [] bodyV
                   (_envT, bodyT) = goBlock envIn body
                   loopT = For iv vecEnd e 1 [] bodyT
               in if vecEnd > s
                  then if vecEnd < e
                       then (env, acc ++ [loopV, loopT])
                       else (env, acc ++ [loopV])
                  else
                    let (_envS, body') = goBlock envIn body
                    in (env, acc ++ [For iv s e stepN carries body'])
             else
               let (_envOut, body') = goBlock envIn body
               in (env, acc ++ [For iv s e stepN carries body'])

    vectorizeBody :: Int -> Map Id Ty -> Id -> [Stmt] -> (Map Id Ty, [Stmt])
    vectorizeBody w0 env0 iv = foldl stepB (env0, [])
      where
        stepB (env, acc) stmt =
          case stmt of
            Let x _ty rhs0 ->
              let rhs1 = rewriteRhs w0 env iv rhs0
                  ty1  = inferTy w env rhs1
                  env' = M.insert x ty1 env
              in (env', acc ++ [Let x ty1 rhs1])

            Store _k buf ix v ->
              let (k1, ix1) = rewriteStore buf ix
              in (env, acc ++ [Store k1 buf ix1 v])

            For{} ->
              (env, acc ++ [stmt])

        rewriteStore :: Buffer -> Expr -> (StoreKind, Expr)
        rewriteStore buf ix =
          case (buf, ix) of
            (InpIdx, Var j) | j == iv -> (StoreContigVec w, Var j)
            (InpVal, Var j) | j == iv -> (StoreContigVec w, Var j)
            _                         -> (StoreScalar, ix)

-- ---------------------------------------------------------------------------
-- Legality: iv must only appear as index into InpIdx / InpVal
-- ---------------------------------------------------------------------------

ivOnlyUsedAsInpIndex :: Id -> [Stmt] -> Bool
ivOnlyUsedAsInpIndex iv = all okStmt
  where
    okStmt stmt =
      case stmt of
        Let _ _ rhs -> okRhs rhs
        Store _ buf ix v -> okStore buf ix && not (usesIv v)
        For{} -> False

    okRhs rhs =
      case rhs of
        RConst _ -> True
        RBin _ a b -> not (usesIv a) && not (usesIv b)
        RMulAdd a b c -> not (usesIv a) && not (usesIv b) && not (usesIv c)
        RSelect c a b -> not (usesIv c) && not (usesIv a) && not (usesIv b)
        RLoad _ buf ix -> okLoad buf ix
        RVBroadcast _ e -> not (usesIv e)
        RReduce _ e -> not (usesIv e)

    okLoad buf ix =
      case (buf, ix) of
        (InpIdx, Var j) | j == iv -> True
        (InpVal, Var j) | j == iv -> True
        _                         -> not (usesIv ix)

    okStore buf ix =
      case (buf, ix) of
        (InpIdx, Var j) | j == iv -> True
        (InpVal, Var j) | j == iv -> True
        _                         -> not (usesIv ix)

    usesIv (Var j)   = j == iv
    usesIv (Const _) = False

-- ---------------------------------------------------------------------------
-- Rewriting + type inference
-- ---------------------------------------------------------------------------

rewriteRhs :: Int -> Map Id Ty -> Id -> Rhs -> Rhs
rewriteRhs w env iv rhs =
  case rhs of
    RConst c        -> RConst c
    RBin op a b     -> RBin op a b
    RMulAdd a b c   -> RMulAdd a b c
    RSelect c a b   -> RSelect c a b
    RVBroadcast bw e -> RVBroadcast bw e
    RReduce op e -> RReduce op e
    RLoad k buf ix ->
      case (k, buf, ix) of
        (LoadScalar, InpIdx, Var j) | j == iv -> RLoad (LoadContigVec w) buf (Var j)
        (LoadScalar, InpVal, Var j) | j == iv -> RLoad (LoadContigVec w) buf (Var j)
        (LoadScalar, Forest, _) | isVec (exprTy env ix) -> RLoad (LoadGatherVec w) buf ix
        _ -> rhs

isVec :: Ty -> Bool
isVec (Vec _ _) = True
isVec _         = False

exprTy :: Map Id Ty -> Expr -> Ty
exprTy env (Var x) = M.findWithDefault I32 x env
exprTy _   (Const _) = I32

inferTy :: Int -> Map Id Ty -> Rhs -> Ty
inferTy w env rhs =
  case rhs of
    RConst _ -> I32

    RLoad k _ _ ->
      case k of
        LoadScalar      -> I32
        LoadContigVec _ -> Vec w I32
        LoadGatherVec _ -> Vec w I32

    RVBroadcast _ _ -> Vec w I32

    RReduce _ _ -> I32

    RBin _ a b -> join2 (exprTy env a) (exprTy env b)

    RMulAdd a b c -> join3 (exprTy env a) (exprTy env b) (exprTy env c)

    RSelect c a b -> join3 (exprTy env c) (exprTy env a) (exprTy env b)
  where
    join2 t1 t2 = if isVec t1 || isVec t2 then Vec w I32 else I32
    join3 t1 t2 t3 = if isVec t1 || isVec t2 || isVec t3 then Vec w I32 else I32
