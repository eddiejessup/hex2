module Hex.Common.TFM.Get.Common where

import ASCII qualified
import Data.Ratio qualified as Ratio
import Effectful.Serialize.Get qualified as Get
import GHC.Num
import Hex.Common.TFM.Types
import Hexlude

-- Read a string that's encoded as an integer, followed by that number of
-- characters.
getBCPL :: Word8 -> (Get.Get :> es, Error TFMError :> es) => Eff es [ASCII.Char]
getBCPL maxLen = do
  getFixedLength (fromIntegral @Word8 @Int maxLen) go
  where
    go _endPointer = do
      n <- Get.getWord8
      when (n > (maxLen - 1)) $ throwError SectionTooLong
      s <- Get.getBytes (fromIntegral @Word8 @Int n)
      case ASCII.byteStringToCharListMaybe s of
        Just v -> pure v
        Nothing -> throwError InvalidBCPL

-- Read a fixed-point fractional value.
getFixWord :: (Get.Get :> es) => Eff es Rational
getFixWord = do
  w <- Get.getWord32be
  pure $ fromIntegral @Word32 @Integer w Ratio.% (2 ^ (20 :: Int))

getFixedLength :: (Get.Get :> es, Error TFMError :> es) => Int -> (Eff es Bool -> Eff es a) -> Eff es a
getFixedLength sectionLength getBody = do
  startPointer <- Get.getBytesRead
  let endPointer = startPointer + sectionLength

      atEndOfSection = do
        curPointer <- Get.getBytesRead
        case compare curPointer endPointer of
          LT -> pure False
          EQ -> pure True
          GT -> throwError SectionTooLong

  body <- getBody atEndOfSection
  donePointer <- Get.getBytesRead
  let bytesRemaining = endPointer - donePointer
  when (bytesRemaining < 0) $ throwError SectionTooLong
  Get.skip bytesRemaining
  pure body

getChunks :: Monad m => m a -> m Bool -> m [a]
getChunks getChunk atEndOfSection = whileM atEndOfSection getChunk

getDesignSizeLength :: (Get.Get :> es) => Eff es LengthDesignSize
getDesignSizeLength = LengthDesignSize <$> getFixWord
