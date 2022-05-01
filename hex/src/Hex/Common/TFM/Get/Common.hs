module Hex.Common.TFM.Get.Common where

import ASCII qualified
import Control.Monad.Fail (fail)
import Data.Ratio qualified as Ratio
import Data.Serialize.Get qualified as Ser
import Hexlude

-- Read a string that's encoded as an integer, followed by that number of
-- characters.
getBCPL :: Word8 -> Ser.Get [ASCII.Char]
getBCPL maxLen =
  do
    n <- Ser.getWord8
    when (n > (maxLen - 1)) (fail $ "BCPL string too long: " <> show n)
    s <- Ser.getBytes (fromIntegral @Word8 @Int n)
    asc <- case ASCII.byteStringToCharListMaybe s of
      Just v -> pure v
      Nothing -> fail "Could not decode ASCII from bytes"
    Ser.skip $ fromIntegral @Word8 @Int (maxLen - 1 - n)
    pure asc

-- Read a fixed-point fractional value.
getFixWord :: Ser.Get Rational
getFixWord = do
  w <- Ser.getWord32be
  pure $ fromIntegral @Word32 @Integer w Ratio.% (2 ^ (20 :: Int))

getChunks :: Ser.Get v -> Ser.Get [v]
getChunks f =
  Ser.isEmpty >>= \case
    True ->
      pure []
    False -> do
      el <- f
      (el :) <$> getChunks f
