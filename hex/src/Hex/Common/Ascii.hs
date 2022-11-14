module Hex.Common.Ascii where

import ASCII qualified as ASCII
import Data.Text.Lazy.Builder qualified as Tx
import Formatting qualified as F
import GHC.Num
import Hexlude

fmtAsciiList :: Fmt [ASCII.Char]
fmtAsciiList = F.later (Tx.fromText . ASCII.charListToText)

ascii :: Char -> Word8
ascii = fromIntegral . ord

fromDecDigit :: Word8 -> Maybe Word8
fromDecDigit w
  | isDecDigit w = Just $ unsafeFromDecDigit w
  | otherwise = Nothing

unsafeFromDecDigit :: Word8 -> Word8
unsafeFromDecDigit w = w - ascii '0'

isDecDigit :: Word8 -> Bool
isDecDigit w = w >= ascii '0' && w <= ascii '9'

isOctDigit :: Word8 -> Bool
isOctDigit w = w >= ascii '0' && w <= ascii '7'

fromOctDigit :: Word8 -> Maybe Word8
fromOctDigit w
  | isOctDigit w = Just $ unsafeFromDecDigit w
  | otherwise = Nothing

unsafeFromUpAF :: Word8 -> Word8
unsafeFromUpAF w = w - ascii 'A' + 10

isUpAF :: Word8 -> Bool
isUpAF w = w >= ascii 'A' && w <= ascii 'F'

fromUpHexDigit :: Word8 -> Maybe Word8
fromUpHexDigit w
  | isDecDigit w = Just $ unsafeFromDecDigit w
  | isUpAF w = Just $ unsafeFromUpAF w
  | otherwise = Nothing

fromUpHexAF :: Word8 -> Maybe Word8
fromUpHexAF w
  | isUpAF w = Just $ unsafeFromUpAF w
  | otherwise = Nothing

isUpper :: Word8 -> Bool
isUpper w = ascii 'A' <= w && w <= ascii 'Z'

isLower :: Word8 -> Bool
isLower w = ascii 'a' <= w && w <= ascii 'z'

toUpper :: Word8 -> Word8
toUpper w
  | isLower w = w - ascii ' '
  | otherwise = w

toLower :: Word8 -> Word8
toLower w
  | isUpper w = w + ascii ' '
  | otherwise = w
