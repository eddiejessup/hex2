module Hex.Common.TFM.Types where

import ASCII qualified
import Hex.Common.Quantity qualified as Q
import Hexlude

data Font = Font
  { checksum :: Word32,
    designFontSize :: Q.LengthScaledPoints Rational,
    characterCodingScheme :: Maybe [ASCII.Char],
    family :: Maybe [ASCII.Char],
    params :: FontParams,
    ligKerns :: [LigKernInstr],
    characters :: IntMap Character
  }
  deriving stock (Show, Generic)

fontLengthScaledPoints :: Font -> Q.LengthDesignSize Rational -> Q.LengthScaledPoints Rational
fontLengthScaledPoints font lengthInDS =
  Q.fromDesignSize lengthInDS (designFontSize font)

fontLengthScaledPointsInt :: Font -> Q.LengthDesignSize Rational -> Q.LengthScaledPoints Int
fontLengthScaledPointsInt font lengthInDS = Q.roundScaledPoints $ fontLengthScaledPoints font lengthInDS

fontLengthParamScaledPoints :: Font -> (Font -> Q.LengthDesignSize Rational) -> Q.LengthScaledPoints Rational
fontLengthParamScaledPoints font getLengthInDS = fontLengthScaledPoints font (getLengthInDS font)

fontLengthParamScaledPointsInt :: Font -> (Font -> Q.LengthDesignSize Rational) -> Q.LengthScaledPoints Int
fontLengthParamScaledPointsInt font getLengthInDS = Q.roundScaledPoints $ fontLengthParamScaledPoints font getLengthInDS

data FontParams = FontParams
  { -- `slant`, the amount of italic slant, which is used to help position
    -- accents. For example, `slant = 0.25` means that when you go up one unit,
    -- you also go `0.25` units to the right. The slant is a pure number; it's
    -- the only `fix_word` other than the design size itself that is not scaled
    -- by the design size.
    slant :: Rational,
    spacing :: Q.LengthDesignSize Rational,
    spaceStretch :: Q.LengthDesignSize Rational,
    spaceShrink :: Q.LengthDesignSize Rational,
    xHeight :: Q.LengthDesignSize Rational,
    quad :: Q.LengthDesignSize Rational,
    extraSpace :: Q.LengthDesignSize Rational,
    extraParams :: Maybe ExtraFontParams
  }
  deriving stock (Show)

data ExtraFontParams
  = MathSymbolFontParams MathSymbolParams
  | MathExtensionFontParams MathExtensionParams
  deriving stock (Show)

data MathSymbolParams = MathSymbolParams
  { num1 :: Q.LengthDesignSize Rational,
    num2 :: Q.LengthDesignSize Rational,
    num3 :: Q.LengthDesignSize Rational,
    denom1 :: Q.LengthDesignSize Rational,
    denom2 :: Q.LengthDesignSize Rational,
    sup1 :: Q.LengthDesignSize Rational,
    sup2 :: Q.LengthDesignSize Rational,
    sup3 :: Q.LengthDesignSize Rational,
    sub1 :: Q.LengthDesignSize Rational,
    sub2 :: Q.LengthDesignSize Rational,
    supdrop :: Q.LengthDesignSize Rational,
    subdrop :: Q.LengthDesignSize Rational,
    delim1 :: Q.LengthDesignSize Rational,
    delim2 :: Q.LengthDesignSize Rational,
    axisHeight :: Q.LengthDesignSize Rational
  }
  deriving stock (Show)

data MathExtensionParams = MathExtensionParams
  { defaultRuleThickness :: Q.LengthDesignSize Rational,
    bigOpSpacing1 :: Q.LengthDesignSize Rational,
    bigOpSpacing2 :: Q.LengthDesignSize Rational,
    bigOpSpacing3 :: Q.LengthDesignSize Rational,
    bigOpSpacing4 :: Q.LengthDesignSize Rational,
    bigOpSpacing5 :: Q.LengthDesignSize Rational
  }
  deriving stock (Show)

newtype KernOp = KernOp (Q.LengthDesignSize Rational)
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
  { width :: Q.LengthDesignSize Rational,
    height :: Q.LengthDesignSize Rational,
    depth :: Q.LengthDesignSize Rational,
    italicCorrection :: Q.LengthDesignSize Rational,
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
