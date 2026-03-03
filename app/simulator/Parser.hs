module Parser
  ( ProgramPayload (..),
    BundlePayload (..),
    SlotPayload (..),
    parseProgramPayload,
    parseProgramPayloadFile,
  )
where

import Control.Applicative (many)
import Control.Monad (zipWithM)
import Data.Char (isDigit, isSpace)
import Text.ParserCombinators.ReadP
  ( ReadP,
    char,
    eof,
    get,
    munch1,
    pfail,
    readP_to_S,
    satisfy,
    skipMany,
    (<++),
  )

data ProgramPayload = ProgramPayload
  { ppRounds :: !Int,
    ppBatchSize :: !Int,
    ppProgram :: ![BundlePayload]
  }
  deriving (Show, Eq)

data BundlePayload = BundlePayload
  { bpAlu :: ![SlotPayload],
    bpValu :: ![SlotPayload],
    bpLoad :: ![SlotPayload],
    bpStore :: ![SlotPayload],
    bpFlow :: ![SlotPayload]
  }
  deriving (Show, Eq)

data SlotPayload = SlotPayload
  { spOp :: !String,
    spArgs :: ![Int]
  }
  deriving (Show, Eq)

data Value
  = VInt !Int
  | VString !String
  | VList ![Value]
  | VTuple ![Value]
  | VDict ![(String, Value)]
  deriving (Show, Eq)

parseProgramPayloadFile :: FilePath -> IO (Either String ProgramPayload)
parseProgramPayloadFile path = parseProgramPayload <$> readFile path

parseProgramPayload :: String -> Either String ProgramPayload
parseProgramPayload src = do
  v <- parseValueFromText src
  decodeProgramPayload v

parseValueFromText :: String -> Either String Value
parseValueFromText src =
  case [v | (v, rest) <- readP_to_S (ws *> parseValue <* ws <* eof) src, null rest] of
    [] -> Left "Parse error: unable to parse payload text"
    xs -> Right (last xs)

parseValue :: ReadP Value
parseValue =
  ws *> (parseDict <++ parseList <++ parseTuple <++ parseStringValue <++ parseIntValue) <* ws

parseIntValue :: ReadP Value
parseIntValue = do
  sign <- (char '-' >> pure negate) <++ ((char '+' >> pure id) <++ pure id)
  digits <- munch1 isDigit
  pure (VInt (sign (read digits)))

parseStringValue :: ReadP Value
parseStringValue = VString <$> parseStringLiteral

parseStringLiteral :: ReadP String
parseStringLiteral = do
  _ <- char '"'
  chars <- many parseStringChar
  _ <- char '"'
  pure chars

parseStringChar :: ReadP Char
parseStringChar = satisfy (\c -> c /= '"' && c /= '\\') <++ parseEscapedChar

parseEscapedChar :: ReadP Char
parseEscapedChar = do
  _ <- char '\\'
  c <- get
  pure $ case c of
    '"' -> '"'
    '\\' -> '\\'
    'n' -> '\n'
    'r' -> '\r'
    't' -> '\t'
    'b' -> '\b'
    'f' -> '\f'
    other -> other

parseList :: ReadP Value
parseList = do
  (items, _) <- parseDelimited '[' ']' parseValue
  pure (VList items)

parseTuple :: ReadP Value
parseTuple = do
  (items, hadTrailingComma) <- parseDelimited '(' ')' parseValue
  case items of
    [] -> pure (VTuple [])
    [_] | not hadTrailingComma -> pfail
    _ -> pure (VTuple items)

parseDict :: ReadP Value
parseDict = do
  (pairs, _) <- parseDelimited '{' '}' parsePair
  pure (VDict pairs)

parsePair :: ReadP (String, Value)
parsePair = do
  key <- parseStringLiteral
  ws
  _ <- char ':'
  val <- parseValue
  pure (key, val)

parseDelimited :: Char -> Char -> ReadP a -> ReadP ([a], Bool)
parseDelimited open close p = do
  _ <- char open
  ws
  (char close >> pure ([], False))
    <++ do
      first <- p
      ws
      parseRest [first]
  where
    parseRest revItems =
      (char close >> pure (reverse revItems, False))
        <++ do
          _ <- char ','
          ws
          (char close >> pure (reverse revItems, True))
            <++ do
              x <- p
              ws
              parseRest (x : revItems)

ws :: ReadP ()
ws = skipMany (satisfy isSpace)

decodeProgramPayload :: Value -> Either String ProgramPayload
decodeProgramPayload v = do
  fields <- expectDict "top-level payload" v
  ensureKnownKeys "top-level payload" ["rounds", "batch_size", "program"] fields
  rounds <- requireInt fields "rounds" "top-level payload"
  batchSize <- requireInt fields "batch_size" "top-level payload"
  programVal <- requireField fields "program" "top-level payload"
  bundles <- expectList "top-level key \"program\"" programVal
  decoded <- zipWithM decodeBundle [0 :: Int ..] bundles
  pure
    ProgramPayload
      { ppRounds = rounds,
        ppBatchSize = batchSize,
        ppProgram = decoded
      }

decodeBundle :: Int -> Value -> Either String BundlePayload
decodeBundle bundleIx v = do
  fields <- expectDict ctx v
  ensureKnownKeys ctx ["alu", "valu", "load", "store", "flow"] fields
  alu <- decodeEngine fields "alu"
  valu <- decodeEngine fields "valu"
  load <- decodeEngine fields "load"
  store <- decodeEngine fields "store"
  flow <- decodeEngine fields "flow"
  pure
    BundlePayload
      { bpAlu = alu,
        bpValu = valu,
        bpLoad = load,
        bpStore = store,
        bpFlow = flow
      }
  where
    ctx = "program bundle #" ++ show bundleIx

    decodeEngine fields engine = do
      maybeVal <- optionalField fields engine ctx
      case maybeVal of
        Nothing -> pure []
        Just slotsVal -> do
          slots <- expectList (ctx ++ " key \"" ++ engine ++ "\"") slotsVal
          zipWithM (decodeSlot engine) [0 :: Int ..] slots

    decodeSlot engine slotIx slotVal =
      case slotVal of
        VTuple [] ->
          Left (slotCtx engine slotIx ++ " must not be empty")
        VTuple (opVal : argsVal) -> do
          op <- expectString (slotCtx engine slotIx ++ " opcode") opVal
          args <- zipWithM (decodeArg engine slotIx) [0 :: Int ..] argsVal
          pure (SlotPayload op args)
        _ ->
          Left (slotCtx engine slotIx ++ " must be a tuple")

    decodeArg engine slotIx argIx =
      expectIntValue (argCtx engine slotIx argIx)

    slotCtx engine slotIx =
      ctx ++ " " ++ engine ++ " slot #" ++ show slotIx

    argCtx engine slotIx argIx =
      slotCtx engine slotIx ++ " arg #" ++ show argIx

expectDict :: String -> Value -> Either String [(String, Value)]
expectDict ctx v =
  case v of
    VDict fields -> Right fields
    _ -> Left (ctx ++ " must be a dict, got " ++ valueTag v)

expectList :: String -> Value -> Either String [Value]
expectList ctx v =
  case v of
    VList xs -> Right xs
    _ -> Left (ctx ++ " must be a list, got " ++ valueTag v)

expectString :: String -> Value -> Either String String
expectString ctx v =
  case v of
    VString s -> Right s
    _ -> Left (ctx ++ " must be a string, got " ++ valueTag v)

expectIntValue :: String -> Value -> Either String Int
expectIntValue ctx v =
  case v of
    VInt n -> Right n
    _ -> Left (ctx ++ " must be an int, got " ++ valueTag v)

requireField :: [(String, Value)] -> String -> String -> Either String Value
requireField fields key ctx =
  case [v | (k, v) <- fields, k == key] of
    [] -> Left (ctx ++ " is missing required key \"" ++ key ++ "\"")
    [v] -> Right v
    _ -> Left (ctx ++ " has duplicate key \"" ++ key ++ "\"")

optionalField :: [(String, Value)] -> String -> String -> Either String (Maybe Value)
optionalField fields key ctx =
  case [v | (k, v) <- fields, k == key] of
    [] -> Right Nothing
    [v] -> Right (Just v)
    _ -> Left (ctx ++ " has duplicate key \"" ++ key ++ "\"")

requireInt :: [(String, Value)] -> String -> String -> Either String Int
requireInt fields key ctx = do
  v <- requireField fields key ctx
  expectIntValue (ctx ++ " key \"" ++ key ++ "\"") v

ensureKnownKeys :: String -> [String] -> [(String, Value)] -> Either String ()
ensureKnownKeys ctx allowed fields =
  case [k | (k, _) <- fields, k `notElem` allowed] of
    [] -> Right ()
    bad -> Left (ctx ++ " has unknown key(s): " ++ show bad)

valueTag :: Value -> String
valueTag v = case v of
  VInt {} -> "int"
  VString {} -> "string"
  VList {} -> "list"
  VTuple {} -> "tuple"
  VDict {} -> "dict"
