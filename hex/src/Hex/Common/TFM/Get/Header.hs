module Hex.Common.TFM.Get.Header where

import ASCII qualified
import Data.Serialize.Get qualified as Ser
import Hex.Common.Quantity qualified as H.Q
import Hex.Common.TFM.Get.Common qualified as H.TFM.Get.Common
import Hexlude

-- The length of the character coding scheme and font family, respectively.
characterCodingSchemeLength :: Word8
characterCodingSchemeLength = 40

familyLength :: Word8
familyLength = 20

-- The information stored in the header table of a H.TFM file.
data Header = Header
  { checksum :: Word32,
    designFontSize :: H.Q.LengthScaledPoints Rational,
    characterCodingScheme :: Maybe [ASCII.Char],
    family :: Maybe [ASCII.Char],
    sevenBitSafeFlag :: Maybe Word8,
    face :: Maybe Face
  }
  deriving stock (Show)

data Face = Face Weight Slope Expansion
  deriving stock (Show)

data Weight = Medium | Bold | Light
  deriving stock (Show)

data Slope = Roman | Italic
  deriving stock (Show)

data Expansion = Regular | Condensed | Extended
  deriving stock (Show)

parseFace :: Word8 -> Maybe Face
parseFace n
  | n > 17 = Nothing
  | otherwise = Just $ Face wt sl ex
  where
    (d6, m6) = n `divMod` 6
    (d2, m2) = m6 `divMod` 2
    wt = case d6 of
      0 -> Medium
      1 -> Bold
      _ -> Light
    ex = case d2 of
      0 -> Regular
      1 -> Condensed
      _ -> Extended
    sl = case m2 of
      0 -> Roman
      _ -> Italic

getHeader :: Ser.Get Header
getHeader =
  do
    -- header[0 ... 1]: Required; checksum and design size.
    checksum <- Ser.getWord32be
    designFontSize <- H.Q.lengthFromPointsRational <$> H.TFM.Get.Common.getFixWord
    -- header[2 ... 11]: Optional; character coding scheme.
    characterCodingScheme <-
      Ser.isEmpty >>= \case
        True -> pure Nothing
        False -> Just <$> H.TFM.Get.Common.getBCPL characterCodingSchemeLength
    -- header[12 ... 16]: Optional; font family.
    family <-
      Ser.isEmpty >>= \case
        True -> pure Nothing
        False -> Just <$> H.TFM.Get.Common.getBCPL familyLength
    -- header[17]: Optional; seven-bit-safe-flag, and face code.
    (sevenBitSafeFlag, face) <-
      Ser.isEmpty >>= \case
        True -> pure (Nothing, Nothing)
        False -> do
          sevenBitSafeFlag <- Ser.getWord8
          _ <- Ser.getWord8
          _ <- Ser.getWord8
          face <- parseFace <$> Ser.getWord8
          pure (Just sevenBitSafeFlag, face)
    pure
      Header
        { checksum,
          designFontSize,
          characterCodingScheme,
          family,
          sevenBitSafeFlag,
          face
        }
