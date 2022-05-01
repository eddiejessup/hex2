module Hex.Common.TFM.Types where

import ASCII qualified
import Hex.Common.Quantity qualified as H.Q
import Hexlude

data Font = Font
  { checksum :: Word32,
    designFontSize :: H.Q.LengthScaledPoints Rational,
    characterCodingScheme :: Maybe [ASCII.Char],
    family :: Maybe [ASCII.Char],
    params :: FontParams,
    ligKerns :: [LigKernInstr],
    characters :: IntMap Character
  }
  deriving stock (Show, Generic)

fontLengthScaledPoints :: Font -> H.Q.LengthDesignSize Rational -> H.Q.LengthScaledPoints Rational
fontLengthScaledPoints font lengthInDS =
  H.Q.fromDesignSize lengthInDS (designFontSize font)

fontLengthScaledPointsInt :: Font -> H.Q.LengthDesignSize Rational -> H.Q.LengthScaledPoints Int
fontLengthScaledPointsInt font lengthInDS = H.Q.roundScaledPoints $ fontLengthScaledPoints font lengthInDS

fontLengthParamScaledPoints :: Font -> (Font -> H.Q.LengthDesignSize Rational) -> H.Q.LengthScaledPoints Rational
fontLengthParamScaledPoints font getLengthInDS = fontLengthScaledPoints font (getLengthInDS font)

fontLengthParamScaledPointsInt :: Font -> (Font -> H.Q.LengthDesignSize Rational) -> H.Q.LengthScaledPoints Int
fontLengthParamScaledPointsInt font getLengthInDS = H.Q.roundScaledPoints $ fontLengthParamScaledPoints font getLengthInDS

data FontParams = FontParams
  { -- `slant`, the amount of italic slant, which is used to help position
    -- accents. For example, `slant = 0.25` means that when you go up one unit,
    -- you also go `0.25` units to the right. The slant is a pure number; it's
    -- the only `fix_word` other than the design size itself that is not scaled
    -- by the design size.
    slant :: Rational,
    spacing :: H.Q.LengthDesignSize Rational,
    spaceStretch :: H.Q.LengthDesignSize Rational,
    spaceShrink :: H.Q.LengthDesignSize Rational,
    xHeight :: H.Q.LengthDesignSize Rational,
    quad :: H.Q.LengthDesignSize Rational,
    extraSpace :: H.Q.LengthDesignSize Rational,
    extraParams :: Maybe ExtraFontParams
  }
  deriving stock (Show)

data ExtraFontParams
  = MathSymbolFontParams MathSymbolParams
  | MathExtensionFontParams MathExtensionParams
  deriving stock (Show)

data MathSymbolParams = MathSymbolParams
  { num1 :: H.Q.LengthDesignSize Rational,
    num2 :: H.Q.LengthDesignSize Rational,
    num3 :: H.Q.LengthDesignSize Rational,
    denom1 :: H.Q.LengthDesignSize Rational,
    denom2 :: H.Q.LengthDesignSize Rational,
    sup1 :: H.Q.LengthDesignSize Rational,
    sup2 :: H.Q.LengthDesignSize Rational,
    sup3 :: H.Q.LengthDesignSize Rational,
    sub1 :: H.Q.LengthDesignSize Rational,
    sub2 :: H.Q.LengthDesignSize Rational,
    supdrop :: H.Q.LengthDesignSize Rational,
    subdrop :: H.Q.LengthDesignSize Rational,
    delim1 :: H.Q.LengthDesignSize Rational,
    delim2 :: H.Q.LengthDesignSize Rational,
    axisHeight :: H.Q.LengthDesignSize Rational
  }
  deriving stock (Show)

data MathExtensionParams = MathExtensionParams
  { defaultRuleThickness :: H.Q.LengthDesignSize Rational,
    bigOpSpacing1 :: H.Q.LengthDesignSize Rational,
    bigOpSpacing2 :: H.Q.LengthDesignSize Rational,
    bigOpSpacing3 :: H.Q.LengthDesignSize Rational,
    bigOpSpacing4 :: H.Q.LengthDesignSize Rational,
    bigOpSpacing5 :: H.Q.LengthDesignSize Rational
  }
  deriving stock (Show)

newtype KernOp = KernOp (H.Q.LengthDesignSize Rational)
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
  { width :: H.Q.LengthDesignSize Rational,
    height :: H.Q.LengthDesignSize Rational,
    depth :: H.Q.LengthDesignSize Rational,
    italicCorrection :: H.Q.LengthDesignSize Rational,
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
