module Hex.Common.HexState.Impl.Font where

import Formatting qualified as F
import Hex.Common.Codes qualified as Code
import Hex.Common.Quantity qualified as Q
import Hex.Common.TFM.Query qualified as TFM
import Hex.Common.TFM.Types qualified as TFM
import Hexlude

data FontInfo = FontInfo
  { fontMetrics :: TFM.Font,
    designScale :: Q.Length,
    hyphenChar :: Q.HexInt,
    skewChar :: Q.HexInt
  }
  deriving stock (Show, Generic)

nullFontInfo :: FontInfo
nullFontInfo =
  FontInfo
    { fontMetrics = TFM.nullFont,
      designScale = TFM.nullFont.designFontSize,
      hyphenChar = Q.HexInt (-1),
      skewChar = Q.HexInt (-1)
    }

fmtFontInfo :: Fmt FontInfo
fmtFontInfo =
  ("hyphenChar: " |%| F.accessed (.hyphenChar) Q.fmtHexInt) |%| "\n"
    <> ("skewChar: " |%| F.accessed (.skewChar) Q.fmtHexInt |%| "\n")
    <> ("TFM font specification: " |%| F.accessed (.fontMetrics) TFM.fmtFont)

data CharacterAttrs = CharacterAttrs
  { width, height, depth, italicCorrection :: Q.Length
  }
  deriving stock (Show, Generic)

characterAttrs :: FontInfo -> Code.CharCode -> Maybe CharacterAttrs
characterAttrs fontInfo charCode = do
  let fontMetrics = fontInfo.fontMetrics
  tfmChar <- TFM.fontCharMetrics fontMetrics charCode
  let toLen :: TFM.LengthDesignSize -> Q.Length
      toLen = lengthFromFontDesignScale fontInfo
  Just
    CharacterAttrs
      { width = toLen tfmChar.width,
        height = toLen tfmChar.height,
        depth = toLen tfmChar.depth,
        italicCorrection = toLen tfmChar.italicCorrection
      }

lengthFromFontDesignScale :: FontInfo -> TFM.LengthDesignSize -> Q.Length
lengthFromFontDesignScale font lengthInDS =
  TFM.lengthFromDesignSize lengthInDS font.designScale

fontLengthParamLength :: FontInfo -> (TFM.Font -> TFM.LengthDesignSize) -> Q.Length
fontLengthParamLength font getLengthInDS = lengthFromFontDesignScale font (getLengthInDS font.fontMetrics)

fontSpaceGlue :: FontInfo -> Q.Glue
fontSpaceGlue fontInfo =
  let spacing = fontLengthParamLength fontInfo (.params.spacing)
      gStretch = Q.FinitePureFlex $ fontLengthParamLength fontInfo (.params.spaceStretch)
      gShrink = Q.FinitePureFlex $ fontLengthParamLength fontInfo (.params.spaceShrink)
   in Q.Glue {Q.gDimen = spacing, Q.gStretch, Q.gShrink}
