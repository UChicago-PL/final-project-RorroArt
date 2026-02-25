module PythonInterop
  ( renderProgramPayload
  ) where

import Data.List (intercalate)
import Data.Word (Word32)
import ISA
  ( AluOp(..)
  , AluSlot(..)
  , Bundle(..)
  , FlowSlot(..)
  , Imm(..)
  , LoadSlot(..)
  , Offset(..)
  , ProgAddr(..)
  , ScratchAddr(..)
  , StoreSlot(..)
  , ValuSlot(..)
  )

renderProgramPayload :: Int -> Int -> [Bundle k] -> String
renderProgramPayload rounds batchSize bundles =
  "{"
    ++ renderKV "rounds" (show rounds) ++ ","
    ++ renderKV "batch_size" (show batchSize) ++ ","
    ++ renderKV "program" (renderList renderBundle bundles)
    ++ "}\n"

renderBundle :: Bundle k -> String
renderBundle b =
  "{" ++ intercalate "," (nonEmptyFields b) ++ "}"
  where
    nonEmptyFields bundle =
      concat
        [ field "alu" renderAluSlot (aluSlots bundle)
        , field "valu" renderValuSlot (valuSlots bundle)
        , field "load" renderLoadSlot (loadSlots bundle)
        , field "store" renderStoreSlot (storeSlots bundle)
        , field "flow" renderFlowSlot (flowSlots bundle)
        ]

    field name renderFn xs =
      if null xs
        then []
        else [renderKV name (renderList renderFn xs)]

renderKV :: String -> String -> String
renderKV key value = show key ++ ":" ++ value

renderList :: (a -> String) -> [a] -> String
renderList f xs = "[" ++ intercalate "," (map f xs) ++ "]"

renderTuple :: [String] -> String
renderTuple [] = "()"
renderTuple [x] = "(" ++ x ++ ",)"
renderTuple xs = "(" ++ intercalate "," xs ++ ")"

renderScratch :: ScratchAddr -> String
renderScratch (ScratchAddr x) = show x

renderProgAddr :: ProgAddr -> String
renderProgAddr (ProgAddr x) = show x

renderImm :: Imm -> String
renderImm (Imm x) = show x

renderOffset :: Offset -> String
renderOffset (Offset x) = show x

renderWord32 :: Word32 -> String
renderWord32 x = show (toInteger x)

aluOpName :: AluOp -> String
aluOpName op = case op of
  Add -> "+"
  Sub -> "-"
  Mul -> "*"
  Div -> "//"
  Cdiv -> "cdiv"
  Xor -> "^"
  And -> "&"
  Or -> "|"
  Shl -> "<<"
  Shr -> ">>"
  Mod -> "%"
  Lt -> "<"
  Eq_ -> "=="

renderAluSlot :: AluSlot -> String
renderAluSlot (Alu op dest a1 a2) =
  renderTuple [show (aluOpName op), renderScratch dest, renderScratch a1, renderScratch a2]

renderValuSlot :: ValuSlot -> String
renderValuSlot slot = case slot of
  VBroadcast dest src ->
    renderTuple [show "vbroadcast", renderScratch dest, renderScratch src]
  MultiplyAdd dest a b c ->
    renderTuple [show "multiply_add", renderScratch dest, renderScratch a, renderScratch b, renderScratch c]
  VAlu op dest a1 a2 ->
    renderTuple [show (aluOpName op), renderScratch dest, renderScratch a1, renderScratch a2]

renderLoadSlot :: LoadSlot -> String
renderLoadSlot slot = case slot of
  Load dest addr ->
    renderTuple [show "load", renderScratch dest, renderScratch addr]
  LoadOffset dest addr off ->
    renderTuple [show "load_offset", renderScratch dest, renderScratch addr, renderOffset off]
  VLoad dest addr ->
    renderTuple [show "vload", renderScratch dest, renderScratch addr]
  Const dest val ->
    renderTuple [show "const", renderScratch dest, renderWord32 val]

renderStoreSlot :: StoreSlot -> String
renderStoreSlot slot = case slot of
  Store addr src ->
    renderTuple [show "store", renderScratch addr, renderScratch src]
  VStore addr src ->
    renderTuple [show "vstore", renderScratch addr, renderScratch src]

renderFlowSlot :: FlowSlot -> String
renderFlowSlot slot = case slot of
  Select dest cond a b ->
    renderTuple [show "select", renderScratch dest, renderScratch cond, renderScratch a, renderScratch b]
  AddImm dest a imm ->
    renderTuple [show "add_imm", renderScratch dest, renderScratch a, renderImm imm]
  VSelect dest cond a b ->
    renderTuple [show "vselect", renderScratch dest, renderScratch cond, renderScratch a, renderScratch b]
  Halt ->
    renderTuple [show "halt"]
  Pause ->
    renderTuple [show "pause"]
  TraceWrite val ->
    renderTuple [show "trace_write", renderScratch val]
  CondJump cond addr ->
    renderTuple [show "cond_jump", renderScratch cond, renderProgAddr addr]
  CondJumpRel cond off ->
    renderTuple [show "cond_jump_rel", renderScratch cond, renderOffset off]
  Jump addr ->
    renderTuple [show "jump", renderProgAddr addr]
  JumpIndirect addr ->
    renderTuple [show "jump_indirect", renderScratch addr]
  CoreId dest ->
    renderTuple [show "coreid", renderScratch dest]
