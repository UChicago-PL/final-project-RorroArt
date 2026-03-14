{-# LANGUAGE OverloadedStrings #-}

module Test.Gen
  ( genStraightLine,
    genWithLoops,
    genWithValu,
    genMixed,
    genAllOps,
  )
where

import Control.Monad (replicateM)
import Control.Monad.Trans.Class (lift)
import Control.Monad.Trans.State.Strict (StateT, evalStateT, get, modify')
import Data.Set (Set)
import Data.Set qualified as Set
import Data.Word (Word32)
import Hedgehog (Gen)
import Hedgehog.Gen qualified as Gen
import Hedgehog.Range qualified as Range
import ISA qualified

-- Generator state: tracks which scratch addrs are readable.

data GenEnv = GenEnv
  { geLive :: !(Set Int),
    geMemAddrs :: !(Set Int),
    geReserved :: !(Set Int),
    geBundleDests :: !(Set Int), -- dests already used in current bundle (avoid write-write conflicts)
    geMemSize :: !Int,
    geScratchSz :: !Int
  }

type GenM = StateT GenEnv Gen

initEnv :: Int -> GenEnv
initEnv memSz = GenEnv Set.empty Set.empty Set.empty Set.empty memSz 1536

-- Helpers

pickFrom :: Set Int -> GenM Int
pickFrom s = lift $ Gen.element (Set.toList s)

pickDest :: GenM Int
pickDest = do
  env <- get
  let sz = geScratchSz env
      avoid = Set.union (geReserved env) (geBundleDests env)
  d <- lift $ Gen.int (Range.constant 0 (sz - 1))
  if Set.member d avoid
    then pickDest
    else do
      modify' $ \e -> e {geBundleDests = Set.insert d (geBundleDests e)}
      pure d

pickVecBase :: GenM Int
pickVecBase = do
  env <- get
  let sz = geScratchSz env
      avoid = Set.union (geReserved env) (geBundleDests env)
  b <- lift $ Gen.int (Range.constant 0 (sz - 8))
  if any (`Set.member` avoid) [b .. b + 7]
    then pickVecBase
    else do
      modify' $ \e -> e {geBundleDests = Set.union (geBundleDests e) (Set.fromList [b .. b + 7])}
      pure b

commitBundle :: ISA.Bundle () -> GenM ()
commitBundle b = modify' $ \e ->
  let ws = bundleWrites b
   in e
        { geLive = Set.union (geLive e) ws,
          geMemAddrs = Set.difference (geMemAddrs e) ws,
          geBundleDests = Set.empty
        }

mk :: Int -> ISA.ScratchAddr
mk = ISA.ScratchAddr

-- Bundle analysis: extract written/read scratch addrs.

bundleWrites :: ISA.Bundle () -> Set Int
bundleWrites b =
  Set.fromList $
    concat
      [ [d | ISA.Alu _ (ISA.ScratchAddr d) _ _ <- ISA.aluSlots b],
        concatMap valuW (ISA.valuSlots b),
        concatMap loadW (ISA.loadSlots b),
        concatMap flowW (ISA.flowSlots b)
      ]
  where
    valuW (ISA.VBroadcast (ISA.ScratchAddr d) _) = [d .. d + 7]
    valuW (ISA.MultiplyAdd (ISA.ScratchAddr d) _ _ _) = [d .. d + 7]
    valuW (ISA.VAlu _ (ISA.ScratchAddr d) _ _) = [d .. d + 7]
    loadW (ISA.Load (ISA.ScratchAddr d) _) = [d]
    loadW (ISA.LoadOffset (ISA.ScratchAddr d) _ _) = [d]
    loadW (ISA.VLoad (ISA.ScratchAddr d) _) = [d .. d + 7]
    loadW (ISA.Const (ISA.ScratchAddr d) _) = [d]
    flowW (ISA.Select (ISA.ScratchAddr d) _ _ _) = [d]
    flowW (ISA.AddImm (ISA.ScratchAddr d) _ _) = [d]
    flowW (ISA.VSelect (ISA.ScratchAddr d) _ _ _) = [d .. d + 7]
    flowW (ISA.CoreId (ISA.ScratchAddr d)) = [d]
    flowW _ = []

-- Slot generators

safeOps :: [ISA.AluOp]
safeOps = [ISA.Add, ISA.Sub, ISA.Mul, ISA.Xor, ISA.And, ISA.Or, ISA.Shl, ISA.Shr, ISA.Lt, ISA.Eq_]

allOps :: [ISA.AluOp]
allOps = [ISA.Add, ISA.Sub, ISA.Mul, ISA.Div, ISA.Cdiv, ISA.Mod, ISA.Xor, ISA.And, ISA.Or, ISA.Shl, ISA.Shr, ISA.Lt, ISA.Eq_]

genConst :: GenM ISA.LoadSlot
genConst = do
  d <- pickDest
  v <- lift $ Gen.word32 Range.constantBounded
  pure $ ISA.Const (mk d) v

genConstMemAddr :: GenM (ISA.LoadSlot, Int)
genConstMemAddr = do
  d <- pickDest
  msz <- geMemSize <$> get
  addr <- lift $ Gen.int (Range.constant 0 (msz - 1))
  pure (ISA.Const (mk d) (fromIntegral addr), d)

genAlu :: GenM ISA.AluSlot
genAlu = do
  live <- geLive <$> get
  op <- lift $ Gen.element safeOps
  a <- pickFrom live
  b <- pickFrom live
  d <- pickDest
  pure $ ISA.Alu op (mk d) (mk a) (mk b)

genLoad :: GenM ISA.LoadSlot
genLoad = do
  ma <- geMemAddrs <$> get
  a <- pickFrom ma
  d <- pickDest
  pure $ ISA.Load (mk d) (mk a)

genStore :: GenM ISA.StoreSlot
genStore = do
  ma <- geMemAddrs <$> get
  live <- geLive <$> get
  a <- pickFrom ma
  s <- pickFrom live
  pure $ ISA.Store (mk a) (mk s)

genSelect :: GenM ISA.FlowSlot
genSelect = do
  live <- geLive <$> get
  c <- pickFrom live
  a <- pickFrom live
  b <- pickFrom live
  d <- pickDest
  pure $ ISA.Select (mk d) (mk c) (mk a) (mk b)

-- Slot limits: alu=12, valu=6, load=2, store=2, flow=1

haltBundle :: ISA.Bundle ()
haltBundle = ISA.Bundle [] [] [] [] [ISA.Halt] []

emptyBundle :: ISA.Bundle ()
emptyBundle = ISA.Bundle [] [] [] [] [] []

-- Seed phase: spread consts across multiple bundles (load limit = 2).
genSeedBundles :: GenM [ISA.Bundle ()]
genSeedBundles = do
  nConst <- lift $ Gen.int (Range.constant 1 3)
  nMem <- lift $ Gen.int (Range.constant 1 2)
  consts <- replicateM nConst genConst
  memResults <- replicateM nMem genConstMemAddr
  let memConsts = map fst memResults
      memRegs = map snd memResults
      allLoads = consts ++ memConsts
  bs <- mapM emitLoadBundle (chunksOf 2 allLoads)
  modify' $ \e -> e {geMemAddrs = Set.union (geMemAddrs e) (Set.fromList memRegs)}
  pure bs
  where
    emitLoadBundle ls = do
      let b = emptyBundle {ISA.loadSlots = ls}
      commitBundle b
      pure b

genBodyBundle :: GenM (ISA.Bundle ())
genBodyBundle = do
  live <- geLive <$> get
  ma <- geMemAddrs <$> get
  let nLive = Set.size live
      hasMem = not (Set.null ma)
  nConst <- lift $ Gen.int (Range.constant 0 1)
  nAlu <- lift $ Gen.int (Range.constant 1 (min 4 nLive))
  nLoad <- if hasMem then lift $ Gen.int (Range.constant 0 (min 1 (2 - nConst))) else pure 0
  nStore <- if hasMem && nLive > 1 then lift $ Gen.int (Range.constant 0 1) else pure 0
  nSel <- if nLive >= 3 then lift $ Gen.int (Range.constant 0 1) else pure 0
  consts <- replicateM nConst genConst
  alus <- replicateM nAlu genAlu
  loads <- replicateM nLoad genLoad
  stores <- replicateM nStore genStore
  sels <- replicateM nSel genSelect
  let b = ISA.Bundle alus [] (consts ++ loads) stores sels []
  commitBundle b
  pure b

chunksOf :: Int -> [a] -> [[a]]
chunksOf _ [] = []
chunksOf n xs = let (h, t) = splitAt n xs in h : chunksOf n t

-- Program generators

genStraightLine :: Gen ([ISA.Bundle ()], [Word32])
genStraightLine = do
  memSz <- Gen.int (Range.constant 16 64)
  mem <- replicateM memSz (Gen.word32 Range.constantBounded)
  n <- Gen.int (Range.constant 2 8)
  bundles <- evalStateT (go n) (initEnv memSz)
  pure (bundles ++ [haltBundle], mem)
  where
    go n = do
      seeds <- genSeedBundles
      ma <- geMemAddrs <$> get
      modify' $ \e -> e {geReserved = ma}
      body <- replicateM n genBodyBundle
      modify' $ \e -> e {geReserved = Set.empty}
      pure (seeds ++ body)

genWithLoops :: Gen ([ISA.Bundle ()], [Word32])
genWithLoops = do
  memSz <- Gen.int (Range.constant 16 64)
  mem <- replicateM memSz (Gen.word32 Range.constantBounded)
  iters <- Gen.int (Range.constant 2 5)
  bundles <- evalStateT (genLoop iters) (initEnv memSz)
  pure (bundles, mem)

genLoop :: Int -> GenM [ISA.Bundle ()]
genLoop iters = do
  counterReg <- pickDest
  let counterConst = ISA.Const (mk counterReg) (fromIntegral iters)
  seedConst <- genConst
  (memConst, memReg) <- genConstMemAddr
  let b0 = emptyBundle {ISA.loadSlots = [counterConst, seedConst]}
  commitBundle b0
  let b1 = emptyBundle {ISA.loadSlots = [memConst]}
  commitBundle b1
  modify' $ \e ->
    e
      { geMemAddrs = Set.insert memReg (geMemAddrs e),
        geReserved = Set.fromList [counterReg, memReg]
      }
  body <- genBodyBundle
  modify' $ \e -> e {geReserved = Set.empty}
  let bDec = emptyBundle {ISA.flowSlots = [ISA.AddImm (mk counterReg) (mk counterReg) (ISA.Imm (-1))]}
  commitBundle bDec
  let bJmp = emptyBundle {ISA.flowSlots = [ISA.CondJump (mk counterReg) (ISA.ProgAddr 2)]}
  commitBundle bJmp
  pure [b0, b1, body, bDec, bJmp, haltBundle]

genWithValu :: Gen ([ISA.Bundle ()], [Word32])
genWithValu = do
  memSz <- Gen.int (Range.constant 16 64)
  mem <- replicateM memSz (Gen.word32 Range.constantBounded)
  bundles <- evalStateT genValuProg (initEnv memSz)
  pure (bundles, mem)

genValuProg :: GenM [ISA.Bundle ()]
genValuProg = do
  baseA <- pickVecBase
  baseB <- pickVecBase
  baseC <- pickVecBase
  addrReg <- pickDest
  scalarReg <- pickDest
  msz <- geMemSize <$> get
  let memBase = max 0 (msz - 8)
  scalarVal <- lift $ Gen.word32 Range.constantBounded
  let b0 =
        emptyBundle
          { ISA.loadSlots =
              [ ISA.Const (mk addrReg) (fromIntegral memBase),
                ISA.Const (mk scalarReg) scalarVal
              ]
          }
  commitBundle b0
  let b1 = emptyBundle {ISA.loadSlots = [ISA.VLoad (mk baseA) (mk addrReg)]}
  commitBundle b1
  let b2 = emptyBundle {ISA.valuSlots = [ISA.VBroadcast (mk baseB) (mk scalarReg)]}
  commitBundle b2
  op <- lift $ Gen.element safeOps
  let b3 = emptyBundle {ISA.valuSlots = [ISA.VAlu op (mk baseC) (mk baseA) (mk baseB)]}
  commitBundle b3
  let b4 = emptyBundle {ISA.storeSlots = [ISA.VStore (mk addrReg) (mk baseC)]}
  commitBundle b4
  pure [b0, b1, b2, b3, b4, haltBundle]

-- All-ops generator: includes div, cdiv, mod with non-zero operands.

genAllOps :: Gen ([ISA.Bundle ()], [Word32])
genAllOps = do
  memSz <- Gen.int (Range.constant 16 64)
  mem <- replicateM memSz (Gen.word32 Range.constantBounded)
  n <- Gen.int (Range.constant 2 6)
  bundles <- evalStateT (goAllOps n) (initEnv memSz)
  pure (bundles ++ [haltBundle], mem)

goAllOps :: Int -> GenM [ISA.Bundle ()]
goAllOps n = do
  seeds <- genNonZeroSeedBundles
  ma <- geMemAddrs <$> get
  modify' $ \e -> e {geReserved = ma}
  body <- replicateM n genAllOpsBundle
  modify' $ \e -> e {geReserved = Set.empty}
  pure (seeds ++ body)

genNonZeroSeedBundles :: GenM [ISA.Bundle ()]
genNonZeroSeedBundles = do
  nConst <- lift $ Gen.int (Range.constant 2 4)
  nMem <- lift $ Gen.int (Range.constant 1 2)
  consts <- replicateM nConst genNonZeroConst
  memResults <- replicateM nMem genConstMemAddr
  let memConsts = map fst memResults
      memRegs = map snd memResults
  bs <- mapM emitLoadBundle (chunksOf 2 (consts ++ memConsts))
  modify' $ \e -> e {geMemAddrs = Set.union (geMemAddrs e) (Set.fromList memRegs)}
  pure bs
  where
    emitLoadBundle ls = do
      let b = emptyBundle {ISA.loadSlots = ls}
      commitBundle b
      pure b

genNonZeroConst :: GenM ISA.LoadSlot
genNonZeroConst = do
  d <- pickDest
  v <- lift $ Gen.word32 (Range.constant 1 maxBound)
  pure $ ISA.Const (mk d) v

genAllOpsBundle :: GenM (ISA.Bundle ())
genAllOpsBundle = do
  live <- geLive <$> get
  ma <- geMemAddrs <$> get
  let nLive = Set.size live
      hasMem = not (Set.null ma)
  nAlu <- lift $ Gen.int (Range.constant 1 (min 4 nLive))
  nLoad <- if hasMem then lift $ Gen.int (Range.constant 0 1) else pure 0
  nStore <- if hasMem && nLive > 1 then lift $ Gen.int (Range.constant 0 1) else pure 0
  alus <- replicateM nAlu genAluAllOps
  loads <- replicateM nLoad genLoad
  stores <- replicateM nStore genStore
  let b = ISA.Bundle alus [] loads stores [] []
  commitBundle b
  pure b

genAluAllOps :: GenM ISA.AluSlot
genAluAllOps = do
  live <- geLive <$> get
  op <- lift $ Gen.element allOps
  a <- pickFrom live
  b <- pickFrom live
  d <- pickDest
  pure $ ISA.Alu op (mk d) (mk a) (mk b)

-- Combined generator: seeds, scalar body, VALU, loop — all in one program.

genMixed :: Gen ([ISA.Bundle ()], [Word32])
genMixed = do
  memSz <- Gen.int (Range.constant 16 64)
  mem <- replicateM memSz (Gen.word32 Range.constantBounded)
  iters <- Gen.int (Range.constant 2 4)
  bundles <- evalStateT (genMixedProg memSz iters) (initEnv memSz)
  pure (bundles, mem)

genMixedProg :: Int -> Int -> GenM [ISA.Bundle ()]
genMixedProg memSz iters = do
  seeds <- genSeedBundles

  addrReg <- pickDest
  let memBase = max 0 (memSz - 8)
  vecA <- pickVecBase
  vecB <- pickVecBase
  let bAddr = emptyBundle {ISA.loadSlots = [ISA.Const (mk addrReg) (fromIntegral memBase)]}
  commitBundle bAddr
  let bVLoad = emptyBundle {ISA.loadSlots = [ISA.VLoad (mk vecA) (mk addrReg)]}
  commitBundle bVLoad
  scalarReg <- do
    live <- geLive <$> get
    pickFrom live
  let bVBcast = emptyBundle {ISA.valuSlots = [ISA.VBroadcast (mk vecB) (mk scalarReg)]}
  commitBundle bVBcast

  counterReg <- pickDest
  let bCounter = emptyBundle {ISA.loadSlots = [ISA.Const (mk counterReg) (fromIntegral iters)]}
  commitBundle bCounter

  let loopTop = length seeds + 4

  ma <- geMemAddrs <$> get
  modify' $ \e -> e {geReserved = Set.unions [Set.fromList [counterReg, addrReg], ma]}
  nBody <- lift $ Gen.int (Range.constant 1 3)
  bodyBundles <- replicateM nBody genMixedBundle
  modify' $ \e -> e {geReserved = Set.empty}

  let bDec = emptyBundle {ISA.flowSlots = [ISA.AddImm (mk counterReg) (mk counterReg) (ISA.Imm (-1))]}
  commitBundle bDec
  let bJmp = emptyBundle {ISA.flowSlots = [ISA.CondJump (mk counterReg) (ISA.ProgAddr loopTop)]}
  commitBundle bJmp

  vecOut <- pickVecBase
  live <- geLive <$> get
  let hasVec = all (`Set.member` live) [vecA .. vecA + 7]
  storeBundles <-
    if hasVec
      then do
        op <- lift $ Gen.element safeOps
        let bVAlu = emptyBundle {ISA.valuSlots = [ISA.VAlu op (mk vecOut) (mk vecA) (mk vecB)]}
        commitBundle bVAlu
        let bVStore = emptyBundle {ISA.storeSlots = [ISA.VStore (mk addrReg) (mk vecOut)]}
        commitBundle bVStore
        pure [bVAlu, bVStore]
      else pure []

  pure $
    seeds
      ++ [bAddr, bVLoad, bVBcast, bCounter]
      ++ bodyBundles
      ++ [bDec, bJmp]
      ++ storeBundles
      ++ [haltBundle]

genMixedBundle :: GenM (ISA.Bundle ())
genMixedBundle = do
  live <- geLive <$> get
  ma <- geMemAddrs <$> get
  let nLive = Set.size live
      hasMem = not (Set.null ma)
      hasVec8 = not (null (findVecBases live))
  nAlu <- lift $ Gen.int (Range.constant 0 (min 3 nLive))
  alus <- replicateM nAlu genAlu
  nValu <- if hasVec8 then lift $ Gen.int (Range.constant 0 1) else pure 0
  valus <- replicateM nValu genVAluSlot
  nConst <- lift $ Gen.int (Range.constant 0 1)
  nLoad <- if hasMem then lift $ Gen.int (Range.constant 0 (min 1 (2 - nConst))) else pure 0
  consts <- replicateM nConst genConst
  loads <- replicateM nLoad genLoad
  nStore <- if hasMem && nLive > 1 then lift $ Gen.int (Range.constant 0 1) else pure 0
  stores <- replicateM nStore genStore
  nSel <- if nLive >= 3 then lift $ Gen.int (Range.constant 0 1) else pure 0
  sels <- replicateM nSel genSelect
  let b = ISA.Bundle alus valus (consts ++ loads) stores sels []
  commitBundle b
  pure b

findVecBases :: Set Int -> [Int]
findVecBases live =
  [b | b <- [0, 8 .. 1528], all (`Set.member` live) [b .. b + 7]]

genVAluSlot :: GenM ISA.ValuSlot
genVAluSlot = do
  live <- geLive <$> get
  let bases = findVecBases live
  srcA <- lift $ Gen.element bases
  srcB <- lift $ Gen.element bases
  dest <- pickVecBase
  op <- lift $ Gen.element safeOps
  pure $ ISA.VAlu op (mk dest) (mk srcA) (mk srcB)
