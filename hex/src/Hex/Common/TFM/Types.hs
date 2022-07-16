module Hex.Common.TFM.Types where

import Formatting qualified as F
import Hex.Common.Quantity qualified as Q
import Hexlude

newtype TFMError = TFMError Text
  deriving stock (Show, Generic)

fmtTfmError :: Fmt TFMError
fmtTfmError = "TFM Error: " |%| F.shown

newtype LengthDesignSize = LengthDesignSize {unLengthDesignSize :: Rational}
  deriving stock (Show, Generic)
  deriving newtype (Eq, Ord)

zeroLengthDesignSize :: LengthDesignSize
zeroLengthDesignSize = LengthDesignSize 0

data Font = Font
  { checksum :: Word32,
    designFontSize :: Q.Length,
    characterCodingScheme :: Maybe Text,
    family :: Maybe Text,
    params :: FontParams,
    ligKerns :: [LigKernInstr],
    characters :: IntMap Character
  }
  deriving stock (Show, Generic)

nullFont :: Font
nullFont =
  Font
    { checksum = 0,
      designFontSize = Q.zeroLength,
      characterCodingScheme = Nothing,
      family = Nothing,
      params = nullFontParams,
      ligKerns = [],
      characters = mempty
    }
  where
    nullFontParams =
      FontParams
        { slant = 0.0,
          spacing = zeroLengthDesignSize,
          spaceStretch = zeroLengthDesignSize,
          spaceShrink = zeroLengthDesignSize,
          xHeight = zeroLengthDesignSize,
          quad = zeroLengthDesignSize,
          extraSpace = zeroLengthDesignSize,
          extraParams = Nothing
        }

fmtFont :: Fmt Font
fmtFont =
  ("Checksum: " |%| F.accessed (.checksum) fmtChecksum |%| "\n")
    <> ("Design font-size: " |%| F.accessed (.designFontSize) Q.fmtLengthWithUnit |%| "\n")
    <> ("Character coding scheme: " |%| F.accessed (.characterCodingScheme) (F.maybed "None" F.stext) |%| "\n")
    <> ("Family: " |%| fmtViewed (field @"family") (F.maybed "None" F.stext) |%| "\n")
    |%| ("Params: [...]\n")
    |%| ("ligKerns: [...]\n")
    |%| ("characters: [...]\n")
  where
    fmtChecksum = F.int

lengthFromFontDesignSize :: Font -> LengthDesignSize -> Q.Length
lengthFromFontDesignSize font lengthInDS =
  lengthFromDesignSize lengthInDS (designFontSize font)

lengthFromDesignSize :: LengthDesignSize -> Q.Length -> Q.Length
lengthFromDesignSize (LengthDesignSize d) = Q.scaleLengthByRational d

fontLengthParamLength :: Font -> (Font -> LengthDesignSize) -> Q.Length
fontLengthParamLength font getLengthInDS = lengthFromFontDesignSize font (getLengthInDS font)

data FontParams = FontParams
  { -- `slant`, the amount of italic slant, which is used to help position
    -- accents. For example, `slant = 0.25` means that when you go up one unit,
    -- you also go `0.25` units to the right. The slant is a pure number; it's
    -- the only `fix_word` other than the design size itself that is not scaled
    -- by the design size.
    slant :: Rational,
    spacing :: LengthDesignSize,
    spaceStretch :: LengthDesignSize,
    spaceShrink :: LengthDesignSize,
    xHeight :: LengthDesignSize,
    quad :: LengthDesignSize,
    extraSpace :: LengthDesignSize,
    extraParams :: Maybe ExtraFontParams
  }
  deriving stock (Show)

data ExtraFontParams
  = MathSymbolFontParams MathSymbolParams
  | MathExtensionFontParams MathExtensionParams
  deriving stock (Show)

data MathSymbolParams = MathSymbolParams
  { num1 :: LengthDesignSize,
    num2 :: LengthDesignSize,
    num3 :: LengthDesignSize,
    denom1 :: LengthDesignSize,
    denom2 :: LengthDesignSize,
    sup1 :: LengthDesignSize,
    sup2 :: LengthDesignSize,
    sup3 :: LengthDesignSize,
    sub1 :: LengthDesignSize,
    sub2 :: LengthDesignSize,
    supdrop :: LengthDesignSize,
    subdrop :: LengthDesignSize,
    delim1 :: LengthDesignSize,
    delim2 :: LengthDesignSize,
    axisHeight :: LengthDesignSize
  }
  deriving stock (Show)

data MathExtensionParams = MathExtensionParams
  { defaultRuleThickness :: LengthDesignSize,
    bigOpSpacing1 :: LengthDesignSize,
    bigOpSpacing2 :: LengthDesignSize,
    bigOpSpacing3 :: LengthDesignSize,
    bigOpSpacing4 :: LengthDesignSize,
    bigOpSpacing5 :: LengthDesignSize
  }
  deriving stock (Show)

newtype KernOp = KernOp (LengthDesignSize)
  deriving stock (Show)

data LigatureOp = LigatureOp
  { ligatureChar :: Word8,
    charsToPassOver :: Word8,
    deleteCurrentChar :: Bool,
    deleteNextChar :: Bool
  }
  deriving stock (Show)

data LigKernOp
  = LigKernLigOp LigatureOp
  | LigKernKernOp KernOp
  deriving stock (Show)

data LigKernInstr = LigKernInstr
  { stop :: Bool,
    nextChar :: Word8,
    operation :: LigKernOp
  }
  deriving stock (Show)

data Character = Character
  { width :: LengthDesignSize,
    height :: LengthDesignSize,
    depth :: LengthDesignSize,
    italicCorrection :: LengthDesignSize,
    special :: Maybe CharacterSpecial
  }
  deriving stock (Show, Generic)

data CharacterSpecial
  = ExtensibleRecipeSpecial Recipe
  | LigKernIndex Word8
  | NextLargerChar Word8
  deriving stock (Show)

data Tag
  = Plain
  | LigKern
  | Chain
  | Extensible
  deriving stock (Show)

data Recipe = Recipe
  { top :: Word8,
    middle :: Word8,
    bottom :: Word8,
    repeater :: Word8
  }
  deriving stock (Show)
