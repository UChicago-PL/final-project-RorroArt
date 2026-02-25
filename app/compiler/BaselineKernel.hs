module BaselineKernel
  ( BaselineDebugKey(..)
  , baselineRounds
  , baselineBatchSize
  , baselineKernel
  , buildBaselineKernel
  ) where

import Control.Monad (forM_)
import Control.Monad.State.Strict
import ISA
  ( AluOp(..)
  , AluSlot(..)
  , Bundle(..)
  , DebugSlot(..)
  , FlowSlot(..)
  , LoadSlot(..)
  , ScratchAddr
  , StoreSlot(..)
  )

data BaselineDebugKey
  = KeyIdx Int Int
  | KeyVal Int Int
  | KeyNodeVal Int Int
  | KeyHashStage Int Int Int
  | KeyHashedVal Int Int
  | KeyNextIdx Int Int
  | KeyWrappedIdx Int Int
  deriving (Show, Eq, Ord)

data BuilderState = BuilderState
  { nextScratch :: !ScratchAddr
  , constMap :: [(Int, ScratchAddr)]
  , instrsRev :: [Bundle BaselineDebugKey]
  }

type BuildM = State BuilderState

baselineRounds :: Int
baselineRounds = 16

baselineBatchSize :: Int
baselineBatchSize = 256

baselineKernel :: [Bundle BaselineDebugKey]
baselineKernel = buildBaselineKernel baselineRounds baselineBatchSize

buildBaselineKernel :: Int -> Int -> [Bundle BaselineDebugKey]
buildBaselineKernel rounds batchSize =
  reverse . instrsRev $ execState (buildKernel rounds batchSize) (BuilderState 0 [] [])

buildKernel :: Int -> Int -> BuildM ()
buildKernel rounds batchSize = do
  tmp1 <- allocScratch
  tmp2 <- allocScratch
  tmp3 <- allocScratch
  roundsAddr <- allocScratch
  nNodesAddr <- allocScratch
  batchSizeAddr <- allocScratch
  forestHeightAddr <- allocScratch
  forestValuesPAddr <- allocScratch
  inpIndicesPAddr <- allocScratch
  inpValuesPAddr <- allocScratch

  let initVars =
        [ roundsAddr
        , nNodesAddr
        , batchSizeAddr
        , forestHeightAddr
        , forestValuesPAddr
        , inpIndicesPAddr
        , inpValuesPAddr
        ]
  forM_ (zip ([0 ..] :: [Int]) initVars) $ \(i, addr) -> do
    emitLoad (Const tmp1 (fromIntegral i))
    emitLoad (Load addr tmp1)

  zeroConst <- scratchConst 0
  oneConst <- scratchConst 1
  twoConst <- scratchConst 2
  emitFlow Pause
  emitDebug (DebugIgnored "Starting loop")

  tmpIdx <- allocScratch
  tmpVal <- allocScratch
  tmpNodeVal <- allocScratch
  tmpAddr <- allocScratch

  forM_ [0 .. rounds - 1] $ \roundI ->
    forM_ [0 .. batchSize - 1] $ \i ->
      buildBatch
        roundI
        i
        tmp1
        tmp2
        tmp3
        tmpIdx
        tmpVal
        tmpNodeVal
        tmpAddr
        nNodesAddr
        inpIndicesPAddr
        inpValuesPAddr
        forestValuesPAddr
        zeroConst
        oneConst
        twoConst

  emitFlow Pause

buildBatch
  :: Int
  -> Int
  -> ScratchAddr
  -> ScratchAddr
  -> ScratchAddr
  -> ScratchAddr
  -> ScratchAddr
  -> ScratchAddr
  -> ScratchAddr
  -> ScratchAddr
  -> ScratchAddr
  -> ScratchAddr
  -> ScratchAddr
  -> ScratchAddr
  -> ScratchAddr
  -> ScratchAddr
  -> BuildM ()
buildBatch
  roundI
  i
  tmp1
  tmp2
  tmp3
  tmpIdx
  tmpVal
  tmpNodeVal
  tmpAddr
  nNodesAddr
  inpIndicesPAddr
  inpValuesPAddr
  forestValuesPAddr
  zeroConst
  oneConst
  twoConst = do
  iConst <- scratchConst i
  emitAlu Add tmpAddr inpIndicesPAddr iConst
  emitLoad (Load tmpIdx tmpAddr)
  emitDebug (Compare tmpIdx (KeyIdx roundI i))

  emitAlu Add tmpAddr inpValuesPAddr iConst
  emitLoad (Load tmpVal tmpAddr)
  emitDebug (Compare tmpVal (KeyVal roundI i))

  emitAlu Add tmpAddr forestValuesPAddr tmpIdx
  emitLoad (Load tmpNodeVal tmpAddr)
  emitDebug (Compare tmpNodeVal (KeyNodeVal roundI i))

  emitAlu Xor tmpVal tmpVal tmpNodeVal
  buildHash roundI i tmpVal tmp1 tmp2
  emitDebug (Compare tmpVal (KeyHashedVal roundI i))

  emitAlu Mod tmp1 tmpVal twoConst
  emitAlu Eq_ tmp1 tmp1 zeroConst
  emitFlow (Select tmp3 tmp1 oneConst twoConst)
  emitAlu Mul tmpIdx tmpIdx twoConst
  emitAlu Add tmpIdx tmpIdx tmp3
  emitDebug (Compare tmpIdx (KeyNextIdx roundI i))

  emitAlu Lt tmp1 tmpIdx nNodesAddr
  emitFlow (Select tmpIdx tmp1 tmpIdx zeroConst)
  emitDebug (Compare tmpIdx (KeyWrappedIdx roundI i))

  emitAlu Add tmpAddr inpIndicesPAddr iConst
  emitStore (Store tmpAddr tmpIdx)

  emitAlu Add tmpAddr inpValuesPAddr iConst
  emitStore (Store tmpAddr tmpVal)

hashStages :: [(AluOp, Int, AluOp, AluOp, Int)]
hashStages =
  [ (Add, 0x7ED55D16, Add, Shl, 12)
  , (Xor, 0xC761C23C, Xor, Shr, 19)
  , (Add, 0x165667B1, Add, Shl, 5)
  , (Add, 0xD3A2646C, Xor, Shl, 9)
  , (Add, 0xFD7046C5, Add, Shl, 3)
  , (Xor, 0xB55A4F09, Xor, Shr, 16)
  ]

buildHash :: Int -> Int -> ScratchAddr -> ScratchAddr -> ScratchAddr -> BuildM ()
buildHash roundI batchI valHashAddr tmp1 tmp2 =
  forM_ (zip [0 ..] hashStages) $ \(stageI, (op1, val1, op2, op3, val3)) -> do
    val1Const <- scratchConst val1
    emitAlu op1 tmp1 valHashAddr val1Const
    val3Const <- scratchConst val3
    emitAlu op3 tmp2 valHashAddr val3Const
    emitAlu op2 valHashAddr tmp1 tmp2
    emitDebug (Compare valHashAddr (KeyHashStage roundI batchI stageI))

allocScratch :: BuildM ScratchAddr
allocScratch = do
  st <- get
  let addr = nextScratch st
  put st { nextScratch = addr + 1 }
  pure addr

scratchConst :: Int -> BuildM ScratchAddr
scratchConst val = do
  st <- get
  case lookupConstAddr val (constMap st) of
    Just addr -> pure addr
    Nothing -> do
      addr <- allocScratch
      modify' (\s -> s { constMap = (val, addr) : constMap s })
      emitLoad (Const addr (fromIntegral val))
      pure addr

lookupConstAddr :: Int -> [(Int, ScratchAddr)] -> Maybe ScratchAddr
lookupConstAddr _ [] = Nothing
lookupConstAddr key ((k, addr) : rest)
  | key == k = Just addr
  | otherwise = lookupConstAddr key rest

emptyBundle :: Bundle BaselineDebugKey
emptyBundle = Bundle [] [] [] [] [] []

emit :: Bundle BaselineDebugKey -> BuildM ()
emit b = modify' (\s -> s { instrsRev = b : instrsRev s })

emitAlu :: AluOp -> ScratchAddr -> ScratchAddr -> ScratchAddr -> BuildM ()
emitAlu op dest a1 a2 = emit $ emptyBundle { aluSlots = [Alu op dest a1 a2] }

emitLoad :: LoadSlot -> BuildM ()
emitLoad slot = emit $ emptyBundle { loadSlots = [slot] }

emitStore :: StoreSlot -> BuildM ()
emitStore slot = emit $ emptyBundle { storeSlots = [slot] }

emitFlow :: FlowSlot -> BuildM ()
emitFlow slot = emit $ emptyBundle { flowSlots = [slot] }

emitDebug :: DebugSlot BaselineDebugKey -> BuildM ()
emitDebug slot = emit $ emptyBundle { debugSlots = [slot] }
