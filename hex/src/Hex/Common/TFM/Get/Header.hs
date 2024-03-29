module Hex.Common.TFM.Get.Header where

import ASCII qualified
import Effectful.Serialize.Get qualified as Get
import Formatting qualified as F
import Hex.Capability.Log.Interface qualified as Log
import Hex.Common.Ascii qualified as H.ASCII
import Hex.Common.Quantity qualified as Q
import Hex.Common.TFM.Get.Common qualified as TFM.Get.Common
import Hex.Common.TFM.Types
import Hexlude

-- The length of the character coding scheme
characterCodingSchemeLength :: Word8
characterCodingSchemeLength = 40

-- The length of the font family.
familyLength :: Word8
familyLength = 20

readCharacterCodingScheme :: [ASCII.Char] -> CharacterCodingScheme
readCharacterCodingScheme x = case ASCII.charListToText x of
  "TeX math symbols" -> MathSymbolsScheme
  "TeX math extension" -> MathExtensionScheme
  "TeX math italic" -> MathItalicScheme
  "TeX text" -> TextScheme
  "TeX text without f-ligatures" -> TextWithoutFLigaturesScheme
  "TeX typewriter text" -> TypewriterTextScheme
  "UNSPECIFIED" -> ExplicitUnspecifiedScheme
  _ -> UnknownScheme x

-- The information stored in the header table of a TFM file.
data Header = Header
  { checksum :: Word32,
    designFontSize :: Q.Length,
    characterCodingScheme :: Maybe CharacterCodingScheme,
    fontFamily :: Maybe [ASCII.Char],
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

getHeader :: (Log.HexLog :> es, Error TFMError :> es, Get.Get :> es) => Eff es Bool -> Eff es Header
getHeader atEndOfTable =
  do
    -- header[0 ... 1]: Required; checksum and design size.
    Log.debugLog "Parsing checksum"
    checksum <- Get.getWord32be
    Log.debugLog $ "Parsed checksum: " <> show checksum
    Log.debugLog "Parsing design size"
    designFontSize <- Q.pt <$> TFM.Get.Common.getFixWord
    Log.debugLog $ F.sformat ("Parsed design size: " |%| Q.fmtLengthWithUnit) designFontSize
    -- header[2 ... 11]: Optional; character coding scheme.
    characterCodingScheme <-
      atEndOfTable >>= \case
        True -> pure Nothing
        False -> do
          Log.debugLog "Parsing character coding scheme"
          scheme <- readCharacterCodingScheme <$> TFM.Get.Common.getBCPL characterCodingSchemeLength
          Log.debugLog $ F.sformat ("Parsed character coding scheme: " |%| fmtCharacterCodingScheme) scheme
          case scheme of
            UnknownScheme x -> Log.warnLog $ F.sformat ("Unknown character coding scheme: " |%| H.ASCII.fmtAsciiList) x
            _ -> pure ()
          pure $ Just scheme

    -- header[12 ... 16]: Optional; font family.
    fontFamily <-
      atEndOfTable >>= \case
        True -> pure Nothing
        False -> Just <$> TFM.Get.Common.getBCPL familyLength
    -- header[17]: Optional; seven-bit-safe-flag, and face code.
    (sevenBitSafeFlag, face) <-
      atEndOfTable >>= \case
        True -> pure (Nothing, Nothing)
        False -> do
          sevenBitSafeFlag <- Get.getWord8
          _ <- Get.getWord8
          _ <- Get.getWord8
          face <- parseFace <$> Get.getWord8
          pure (Just sevenBitSafeFlag, face)
    pure
      Header
        { checksum,
          designFontSize,
          characterCodingScheme,
          fontFamily,
          sevenBitSafeFlag,
          face
        }
