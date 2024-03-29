module Hex.Common.HexState.Impl.Font where

import Formatting qualified as F
import Hex.Common.Codes qualified as Code
import Hex.Common.HexState.Interface.Font qualified as HSt.Font
import Hex.Common.Quantity qualified as Q
import Hex.Common.TFM.Query qualified as TFM
import Hex.Common.TFM.Types qualified as TFM
import Hexlude

data FontInfo = FontInfo
  { fontMetrics :: TFM.Font,
    designScale :: Q.Length,
    hyphenChar :: Q.HexInt,
    skewChar :: Q.HexInt,
    fontPath :: HexFilePath
  }
  deriving stock (Show, Generic)

nullFontInfo :: FontInfo
nullFontInfo =
  FontInfo
    { fontMetrics = TFM.nullFont,
      designScale = TFM.nullFont.designFontSize,
      hyphenChar = Q.HexInt (-1),
      skewChar = Q.HexInt (-1),
      fontPath = HexFilePath ""
    }

fmtFontInfo :: Fmt FontInfo
fmtFontInfo =
  ("hyphenChar: " |%| F.accessed (.hyphenChar) Q.fmtHexInt) |%| "\n"
    <> ("skewChar: " |%| F.accessed (.skewChar) Q.fmtHexInt |%| "\n")
    <> ("TFM font specification: " |%| F.accessed (.fontMetrics) TFM.fmtFont)

characterAttrs :: FontInfo -> Code.CharCode -> Maybe HSt.Font.CharacterAttrs
characterAttrs fontInfo charCode = do
  let fontMetrics = fontInfo.fontMetrics
  tfmChar <- TFM.fontCharMetrics fontMetrics charCode
  let toLen :: TFM.LengthDesignSize -> Q.Length
      toLen = lengthFromFontDesignScale fontInfo
  Just
    HSt.Font.CharacterAttrs
      { dims = tfmChar.dims <&> toLen,
        italicCorrection = toLen tfmChar.italicCorrection
      }

fontInfoLengthParam :: FontInfo -> TFM.FontLengthParam -> Q.Length
fontInfoLengthParam font param =
  TFM.fontParam font.fontMetrics param font.designScale

lengthFromFontDesignScale :: FontInfo -> TFM.LengthDesignSize -> Q.Length
lengthFromFontDesignScale font lengthInDS =
  TFM.lengthFromDesignSize lengthInDS font.designScale
