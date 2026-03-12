module Parser
  ( ProgramTyped (..),
    parseProgramPayloadFileTyped,
    parseProgramPayloadFileTypedWith,
  )
where

import Data.Char (isDigit, isSpace)
import Data.Maybe (fromMaybe)
import Data.Word (Word32)
import ISA qualified

-- Output type

data ProgramTyped = ProgramTyped
  { ptRounds :: !Int,
    ptBatchSize :: !Int,
    ptProgram :: ![ISA.Bundle ()]
  }
  deriving (Show, Eq)

-- File I/O

parseProgramPayloadFileTyped :: FilePath -> IO (Either String ProgramTyped)
parseProgramPayloadFileTyped = parseProgramPayloadFileTypedWith ISA.defaultMachineConfig

parseProgramPayloadFileTypedWith :: ISA.MachineConfig -> FilePath -> IO (Either String ProgramTyped)
parseProgramPayloadFileTypedWith cfg path = parseProgram cfg <$> readFile path

-- Hand-rolled recursive descent parser.
-- P a = remaining input -> Either error (result, rest)

type P a = String -> Either String (a, String)

-- Whitespace and characters

ws :: String -> String
ws = dropWhile isSpace

ch :: Char -> P ()
ch c s = case ws s of
  (x : rest) | x == c -> Right ((), rest)
  _ -> Left $ "expected '" ++ [c] ++ "'"

-- Comma-separated items between open/close delimiters
commaSep :: Char -> Char -> P a -> P [a]
commaSep open close item s0 = do
  ((), s1) <- ch open s0
  case ws s1 of
    (x : _) | x == close -> Right ([], drop 1 (ws s1))
    _ -> go [] s1
  where
    go acc s = do
      (x, s1) <- item s
      let s2 = ws s1
      case s2 of
        (c : rest)
          | c == close -> Right (reverse (x : acc), rest)
          | c == ',' -> go (x : acc) (ws rest)
        _ -> Left $ "expected ',' or '" ++ [close] ++ "'"

-- Primitives

pString :: P String
pString s0 = case ws s0 of
  ('"' : rest) -> goStr [] rest
  _ -> Left "expected string"
  where
    goStr acc ('"' : rest) = Right (reverse acc, rest)
    goStr acc ('\\' : c : rest) = goStr (esc c : acc) rest
    goStr acc (c : rest) = goStr (c : acc) rest
    goStr _ [] = Left "unterminated string"
    esc 'n' = '\n'
    esc 't' = '\t'
    esc c = c

pInt :: P Int
pInt s0 = case ws s0 of
  ('-' : rest) -> do
    (n, s) <- digits rest
    Right (negate n, s)
  s@(c : _) | isDigit c -> digits s
  _ -> Left "expected int"
  where
    digits s =
      let (ds, rest) = span isDigit s
       in if null ds
            then Left "expected digits"
            else Right (read ds, rest)

-- Slot: ("op", arg1, arg2, ...)
-- Returns (opcode, [int args])

pSlot :: P (String, [Int])
pSlot s0 = do
  ((), s1) <- ch '(' s0
  (op, s2) <- pString s1
  (args, s3) <- slotArgs s2
  Right ((op, args), s3)
  where
    slotArgs s = case ws s of
      (')' : rest) -> Right ([], rest)
      (',' : rest) -> case ws rest of
        (')' : rest2) -> Right ([], rest2) -- trailing comma: ("halt",)
        _ -> goArgs [] rest
      _ -> Left "expected ',' or ')' in slot"
    goArgs acc s = do
      (n, s1) <- pInt s
      case ws s1 of
        (')' : rest) -> Right (reverse (n : acc), rest)
        (',' : rest) -> case ws rest of
          (')' : rest2) -> Right (reverse (n : acc), rest2)
          _ -> goArgs (n : acc) rest
        _ -> Left "expected ',' or ')' in slot args"

-- Bundle: {"engine": [slots, ...], ...}

pBundle :: ISA.MachineConfig -> Int -> P (ISA.Bundle ())
pBundle cfg ix s0 = do
  (pairs, s1) <- commaSep '{' '}' pEnginePair s0
  decodeBundle cfg ix pairs s1

pEnginePair :: P (String, [(String, [Int])])
pEnginePair s0 = do
  (key, s1) <- pString s0
  ((), s2) <- ch ':' s1
  (slots, s3) <- commaSep '[' ']' pSlot s2
  Right ((key, slots), s3)

-- Top-level: {"rounds": N, "batch_size": N, "program": [...]}

parseProgram :: ISA.MachineConfig -> String -> Either String ProgramTyped
parseProgram cfg src = do
  ((), s0) <- ch '{' src
  (rounds, s1) <- field "rounds" pInt s0
  ((), s2) <- ch ',' s1
  (batchSize, s3) <- field "batch_size" pInt s2
  ((), s4) <- ch ',' s3
  (_, s5) <- expectKey "program" s4
  ((), s6) <- ch '[' s5
  (bundles, s7) <- parseBundles cfg 0 [] s6
  ((), s8) <- ch '}' s7
  let rest = ws s8
  if null rest
    then Right ProgramTyped {ptRounds = rounds, ptBatchSize = batchSize, ptProgram = bundles}
    else Left $ "trailing input after program"
  where
    field key p s = do
      (_, s1) <- expectKey key s
      p s1

    expectKey key s0 = do
      (k, s1) <- pString (ws s0)
      if k == key
        then do ((), s2) <- ch ':' s1; Right (k, s2)
        else Left $ "expected key \"" ++ key ++ "\", got \"" ++ k ++ "\""

    parseBundles _ _ acc s = case ws s of
      (']' : rest) -> Right (reverse acc, rest)
      _ -> do
        let ix = length acc
        (b, s1) <- pBundle cfg ix s
        case ws s1 of
          (']' : rest) -> Right (reverse (b : acc), rest)
          (',' : rest) -> parseBundles cfg (ix + 1) (b : acc) rest
          _ -> Left $ "expected ',' or ']' after bundle #" ++ show ix

decodeBundle :: ISA.MachineConfig -> Int -> [(String, [(String, [Int])])] -> String -> Either String (ISA.Bundle (), String)
decodeBundle cfg ix pairs rest = do
  let lim = ISA.mcSlotLimits cfg
      find key = fromMaybe [] (lookup key pairs)
      err engine = "bundle #" ++ show ix ++ " " ++ engine
  alu <- mapM (decodeAlu ix) (zip [0 ..] (find "alu"))
  checkLen (err "alu") (ISA.slAlu lim) alu
  valu <- mapM (decodeValu ix) (zip [0 ..] (find "valu"))
  checkLen (err "valu") (ISA.slValu lim) valu
  load <- mapM (decodeLoad ix) (zip [0 ..] (find "load"))
  checkLen (err "load") (ISA.slLoad lim) load
  store <- mapM (decodeStore ix) (zip [0 ..] (find "store"))
  checkLen (err "store") (ISA.slStore lim) store
  flow <- mapM (decodeFlow ix) (zip [0 ..] (find "flow"))
  checkLen (err "flow") (ISA.slFlow lim) flow
  let unknown = [k | (k, _) <- pairs, k `notElem` ["alu", "valu", "load", "store", "flow"]]
  case unknown of
    [] -> Right (ISA.Bundle alu valu load store flow [], rest)
    _ -> Left $ "bundle #" ++ show ix ++ ": unknown engines " ++ show unknown
  where
    checkLen msg limit xs
      | length xs <= limit = Right ()
      | otherwise = Left $ msg ++ " exceeds slot limit " ++ show limit

-- Per-engine slot decoders

sa :: Int -> ISA.ScratchAddr
sa = ISA.ScratchAddr

pa :: Int -> ISA.ProgAddr
pa = ISA.ProgAddr

w32 :: Int -> Word32
w32 n = fromInteger (toInteger n `mod` (2 ^ (32 :: Integer)))

ctx :: Int -> String -> Int -> String
ctx bix engine six = "bundle #" ++ show bix ++ " " ++ engine ++ " #" ++ show six

decodeAlu :: Int -> (Int, (String, [Int])) -> Either String ISA.AluSlot
decodeAlu bix (six, (op, args)) = case (ISA.parseAluOp op, args) of
  (Just aop, [d, a, b]) -> Right $ ISA.Alu aop (sa d) (sa a) (sa b)
  _ -> Left $ ctx bix "alu" six ++ ": bad slot (" ++ op ++ ", " ++ show args ++ ")"

decodeValu :: Int -> (Int, (String, [Int])) -> Either String ISA.ValuSlot
decodeValu bix (six, (op, args)) = case (op, args) of
  ("vbroadcast", [d, s]) -> Right $ ISA.VBroadcast (sa d) (sa s)
  ("multiply_add", [d, a, b, c]) -> Right $ ISA.MultiplyAdd (sa d) (sa a) (sa b) (sa c)
  _ -> case (ISA.parseAluOp op, args) of
    (Just aop, [d, a, b]) -> Right $ ISA.VAlu aop (sa d) (sa a) (sa b)
    _ -> Left $ ctx bix "valu" six ++ ": bad slot (" ++ op ++ ", " ++ show args ++ ")"

decodeLoad :: Int -> (Int, (String, [Int])) -> Either String ISA.LoadSlot
decodeLoad bix (six, (op, args)) = case (op, args) of
  ("load", [d, a]) -> Right $ ISA.Load (sa d) (sa a)
  ("load_offset", [d, a, o]) -> Right $ ISA.LoadOffset (sa d) (sa a) (ISA.Offset o)
  ("vload", [d, a]) -> Right $ ISA.VLoad (sa d) (sa a)
  ("const", [d, v]) -> Right $ ISA.Const (sa d) (w32 v)
  _ -> Left $ ctx bix "load" six ++ ": bad slot (" ++ op ++ ", " ++ show args ++ ")"

decodeStore :: Int -> (Int, (String, [Int])) -> Either String ISA.StoreSlot
decodeStore bix (six, (op, args)) = case (op, args) of
  ("store", [a, s]) -> Right $ ISA.Store (sa a) (sa s)
  ("vstore", [a, s]) -> Right $ ISA.VStore (sa a) (sa s)
  _ -> Left $ ctx bix "store" six ++ ": bad slot (" ++ op ++ ", " ++ show args ++ ")"

decodeFlow :: Int -> (Int, (String, [Int])) -> Either String ISA.FlowSlot
decodeFlow bix (six, (op, args)) = case (op, args) of
  ("select", [d, c, a, b]) -> Right $ ISA.Select (sa d) (sa c) (sa a) (sa b)
  ("add_imm", [d, a, i]) -> Right $ ISA.AddImm (sa d) (sa a) (ISA.Imm i)
  ("vselect", [d, c, a, b]) -> Right $ ISA.VSelect (sa d) (sa c) (sa a) (sa b)
  ("halt", []) -> Right ISA.Halt
  ("pause", []) -> Right ISA.Pause
  ("trace_write", [v]) -> Right $ ISA.TraceWrite (sa v)
  ("cond_jump", [c, a]) -> Right $ ISA.CondJump (sa c) (pa a)
  ("cond_jump_rel", [c, o]) -> Right $ ISA.CondJumpRel (sa c) (ISA.Offset o)
  ("jump", [a]) -> Right $ ISA.Jump (pa a)
  ("jump_indirect", [a]) -> Right $ ISA.JumpIndirect (sa a)
  ("coreid", [d]) -> Right $ ISA.CoreId (sa d)
  _ -> Left $ ctx bix "flow" six ++ ": bad slot (" ++ op ++ ", " ++ show args ++ ")"
