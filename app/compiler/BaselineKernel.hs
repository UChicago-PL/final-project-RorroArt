module BaselineKernel
  ( scalarKernel
  , hashStages
  ) where

import Data.Word (Word32)

import ISA (AluOp(..))
import FinalIR

-- | The 6-stage mix pipeline from the harness (matches HASH_STAGES).
hashStages :: [(AluOp, Word32, AluOp, AluOp, Word32)]
hashStages =
  [ (Add, 0x7ED55D16, Add, Shl, 12)
  , (Xor, 0xC761C23C, Xor, Shr, 19)
  , (Add, 0x165667B1, Add, Shl, 5)
  , (Add, 0xD3A2646C, Xor, Shl, 9)
  , (Add, 0xFD7046C5, Add, Shl, 3)
  , (Xor, 0xB55A4F09, Xor, Shr, 16)
  ]

-- | Build the scalar baseline kernel.
scalarKernel :: Int -> Int -> Function
scalarKernel rounds batchSize =
  let (_unit, stmts) = runBuild $ do
        -- Header loads (slots 0..6).
        _roundsV <- emitLet I32 (RLoad LoadScalar Header (Const 0))
        nNodes   <- emitLet I32 (RLoad LoadScalar Header (Const 1))
        _batch   <- emitLet I32 (RLoad LoadScalar Header (Const 2))
        _hgt     <- emitLet I32 (RLoad LoadScalar Header (Const 3))
        _fptr    <- emitLet I32 (RLoad LoadScalar Header (Const 4))
        _iptr    <- emitLet I32 (RLoad LoadScalar Header (Const 5))
        _vptr    <- emitLet I32 (RLoad LoadScalar Header (Const 6))

        emitFor 0 rounds 1 $ \_r -> do
          emitFor 0 batchSize 1 $ \i -> do
            idx  <- emitLet I32 (RLoad LoadScalar InpIdx i)
            val0 <- emitLet I32 (RLoad LoadScalar InpVal i)
            node <- emitLet I32 (RLoad LoadScalar Forest idx)
            valX <- emitLet I32 (RBin Xor val0 node)
            valH <- emitHash valX
            mod2 <- emitLet I32 (RBin Mod valH (Const 2))
            isEv <- emitLet I32 (RBin Eq_ mod2 (Const 0))
            add  <- emitLet I32 (RSelect isEv (Const 1) (Const 2))
            idx2 <- emitLet I32 (RBin Mul idx (Const 2))
            idxN <- emitLet I32 (RBin Add idx2 add)
            ok   <- emitLet I32 (RBin Lt idxN nNodes)
            idxW <- emitLet I32 (RSelect ok idxN (Const 0))
            emitStore StoreScalar InpIdx i idxW
            emitStore StoreScalar InpVal i valH

        pure ()
  in Function stmts
  where
    emitHash :: Expr -> Build Expr
    emitHash a0 = foldl go (pure a0) hashStages
      where
        go ma (op1, c1, op2, op3, c3) = do
          a  <- ma
          t1 <- emitLet I32 (RBin op1 a (Const c1))
          t2 <- emitLet I32 (RBin op3 a (Const c3))
          emitLet I32 (RBin op2 t1 t2)
