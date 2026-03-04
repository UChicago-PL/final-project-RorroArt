module Sim.MemoryIO
  ( readMemFile,
    writeMemFile,
    word32ToInt,
    intToWord32,
  )
where

import Control.Monad (when)
import Data.Bits (shiftL, shiftR, (.|.))
import Data.ByteString qualified as BS
import Data.Word (Word32, Word8)

readMemFile :: FilePath -> IO [Word32]
readMemFile path = do
  bs <- BS.readFile path
  let n = BS.length bs
  when
    (n `mod` 4 /= 0)
    (ioError (userError ("Memory file size must be a multiple of 4 bytes, got " ++ show n)))
  pure [decodeWord32LEAt bs i | i <- [0, 4 .. n - 4]]

writeMemFile :: FilePath -> [Word32] -> IO ()
writeMemFile path ws =
  BS.writeFile path (BS.pack (concatMap encodeWord32LE ws))

word32ToInt :: Word32 -> Int
word32ToInt = fromIntegral

intToWord32 :: Int -> Word32
intToWord32 n = fromIntegral (toInteger n `mod` (2 ^ (32 :: Integer)))

decodeWord32LEAt :: BS.ByteString -> Int -> Word32
decodeWord32LEAt bs i =
  fromIntegral (byte 0)
    .|. (fromIntegral (byte 1) `shiftL` 8)
    .|. (fromIntegral (byte 2) `shiftL` 16)
    .|. (fromIntegral (byte 3) `shiftL` 24)
  where
    byte off = BS.index bs (i + off)

encodeWord32LE :: Word32 -> [Word8]
encodeWord32LE w =
  [ fromIntegral w,
    fromIntegral (w `shiftR` 8),
    fromIntegral (w `shiftR` 16),
    fromIntegral (w `shiftR` 24)
  ]
