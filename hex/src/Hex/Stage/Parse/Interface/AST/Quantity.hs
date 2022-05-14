{-# LANGUAGE UndecidableInstances #-}

module Hex.Stage.Parse.Interface.AST.Quantity where

import Hex.Common.HexState.Interface.Resolve.PrimitiveToken qualified as PT
import Hex.Common.Quantity qualified as Q
import Hexlude

data Signed a = Signed [Q.Sign] a
  deriving stock (Show, Eq, Generic)

newtype HexInt = HexInt {unInt :: Signed UnsignedInt}
  deriving stock (Show, Eq, Generic)

data UnsignedInt
  = NormalUnsignedInt NormalInt
  | CoercedUnsignedInt CoercedInt
  deriving stock (Show, Eq, Generic)

data IntBase = Base8 | Base10 | Base16
  deriving stock (Show, Eq, Generic)

data IntConstantDigits = IntConstantDigits {intBase :: IntBase, digits :: [Word8]}
  deriving stock (Show, Eq, Generic)

-- Think: 'un-coerced integer'.
data NormalInt
  = IntConstant IntConstantDigits
  | CharLikeCode Word8
  | InternalInt InternalInt
  deriving stock (Show, Eq, Generic)

zeroInt :: NormalInt
zeroInt = IntConstant $ IntConstantDigits Base10 [0]

oneInt :: NormalInt
oneInt = IntConstant $ IntConstantDigits Base10 [1]

data CoercedInt
  = InternalLengthAsInt InternalLength
  | InternalGlueAsInt InternalGlue
  deriving stock (Show, Eq, Generic)

-- Length.

newtype Length = Length {unLength :: Signed UnsignedLength}
  deriving stock (Show, Eq, Generic)

zeroLength :: Length
zeroLength =
  Length $
    Signed [Q.Positive] $
      NormalLengthAsULength $
        LengthSemiConstant zeroFactor scaledPointUnit

data UnsignedLength
  = NormalLengthAsULength NormalLength
  | CoercedLength CoercedLength
  deriving stock (Show, Eq, Generic)

-- Think: 'un-coerced length'.
data NormalLength
  = -- 'semi-constant' because Factor and Unit can be quite un-constant-like.
    LengthSemiConstant Factor Unit
  | InternalLength InternalLength
  deriving stock (Show, Eq, Generic)

data Factor
  = NormalIntFactor NormalInt
  | -- Badly named 'decimal constant' in the TeXbook. Granted, it is specified
    -- with decimal digits, but its main feature is that it can represent
    -- non-integers.
    DecimalFractionFactor DecimalFraction
  deriving stock (Show, Eq, Generic)

zeroFactor, oneFactor :: Factor
zeroFactor = NormalIntFactor zeroInt
oneFactor = NormalIntFactor oneInt

data DecimalFraction = DecimalFraction {wholeDigits :: [Word8], fracDigits :: [Word8]}
  deriving stock (Show, Eq, Generic)

data Unit
  = PhysicalUnit PhysicalUnitFrame Q.PhysicalUnit
  | InternalUnit InternalUnit
  deriving stock (Show, Eq, Generic)

scaledPointUnit :: Unit
scaledPointUnit = PhysicalUnit MagnifiedFrame Q.ScaledPoint

data InternalUnit
  = Em
  | Ex
  | InternalIntUnit InternalInt
  | InternalLengthUnit InternalLength
  | InternalGlueUnit InternalGlue
  deriving stock (Show, Eq, Generic)

data PhysicalUnitFrame = MagnifiedFrame | TrueFrame
  deriving stock (Show, Eq, Generic)

newtype CoercedLength = InternalGlueAsLength InternalGlue
  deriving stock (Show, Eq, Generic)

-- Math-length.
type MathLength = Signed UnsignedMathLength

data UnsignedMathLength
  = NormalMathLengthAsUMathLength NormalMathLength
  | CoercedMathLength CoercedMathLength
  deriving stock (Show, Eq, Generic)

-- Think: 'un-coerced length'.
data NormalMathLength
  = -- 'semi-constant' because Factor and Unit can be quite un-constant-like.
    MathLengthSemiConstant Factor MathUnit
  deriving stock (Show, Eq, Generic)

data MathUnit = Mu | InternalMathGlueAsUnit InternalMathGlue
  deriving stock (Show, Eq, Generic)

newtype CoercedMathLength = InternalMathGlueAsMathLength InternalMathGlue
  deriving stock (Show, Eq, Generic)

-- Glue.
data Glue
  = ExplicitGlue ExplicitGlueSpec
  | InternalGlue (Signed InternalGlue)
  deriving stock (Show, Eq, Generic)

data ExplicitGlueSpec = ExplicitGlueSpec {egLength :: Length, egStretch :: Maybe PureFlex, egShrink :: Maybe PureFlex}
  deriving stock (Show, Eq, Generic)

data PureFlex
  = FinitePureFlex Length
  | InfPureFlex InfFlexOfOrder
  deriving stock (Show, Eq, Generic)

oneFilFlex, minusOneFilFlex, oneFillFlex :: PureFlex
oneFilFlex = InfPureFlex oneFil
minusOneFilFlex = InfPureFlex minusOneFil
oneFillFlex = InfPureFlex oneFill

data InfFlexOfOrder = InfFlexOfOrder (Signed Factor) Q.InfFlexOrder
  deriving stock (Show, Eq, Generic)

oneFil, minusOneFil, oneFill :: InfFlexOfOrder
oneFil = InfFlexOfOrder (Signed [Q.Positive] oneFactor) Q.Fil1
minusOneFil = InfFlexOfOrder (Signed [Q.Negative] oneFactor) Q.Fil1
oneFill = InfFlexOfOrder (Signed [Q.Positive] oneFactor) Q.Fil2

-- Math glue.
data MathGlue
  = ExplicitMathGlue MathLength (Maybe MathFlex) (Maybe MathFlex)
  | InternalMathGlue (Signed InternalMathGlue)
  deriving stock (Show, Eq, Generic)

data MathFlex = FiniteMathFlex MathLength | FilMathFlex InfFlexOfOrder
  deriving stock (Show, Eq, Generic)

type family QuantParam (a :: PT.QuantityType) where
  QuantParam 'PT.IntQuantity = PT.IntParameter
  QuantParam 'PT.LengthQuantity = PT.LengthParameter
  QuantParam 'PT.GlueQuantity = PT.GlueParameter
  QuantParam 'PT.MathGlueQuantity = PT.MathGlueParameter
  QuantParam 'PT.TokenListQuantity = PT.TokenListParameter

-- Internal quantities.
data QuantVariableAST (a :: PT.QuantityType) = ParamVar (QuantParam a) | RegisterVar RegisterLocation
  deriving stock (Generic)

deriving stock instance Show (QuantParam a) => Show (QuantVariableAST a)

deriving stock instance Eq (QuantParam a) => Eq (QuantVariableAST a)

data RegisterLocation = ExplicitRegisterLocation HexInt | InternalRegisterLocation Q.HexInt
  deriving stock (Show, Eq, Generic)

data InternalInt
  = InternalIntVariable (QuantVariableAST 'PT.IntQuantity)
  | InternalSpecialIntParameter PT.SpecialIntParameter
  | InternalCodeTableRef CodeTableRef
  | InternalCharToken Q.HexInt
  | InternalMathCharToken Q.HexInt
  | InternalFontCharRef FontCharRef
  | LastPenalty
  | ParShape
  | InputLineNr
  | Badness
  deriving stock (Show, Eq, Generic)

data CodeTableRef = CodeTableRef {codeType :: PT.CodeType, codeIndex :: CharCodeInt}
  deriving stock (Show, Eq, Generic)

-- | Newtype wrapper to represent HexInts that represent a char-code.
newtype CharCodeInt = CharCodeInt {unCharCodeInt :: HexInt}
  deriving stock (Show, Eq, Generic)

data FontCharRef = FontCharRef PT.FontChar FontRef
  deriving stock (Show, Eq, Generic)

data FontRef
  = FontTokenRef PT.FontNumber
  | CurrentFontRef
  | FamilyMemberFontRef FamilyMember
  deriving stock (Show, Eq, Generic)

data FamilyMember = FamilyMember PT.FontRange HexInt
  deriving stock (Show, Eq, Generic)

data BoxDimensionRef = BoxDimensionRef HexInt Q.BoxDim
  deriving stock (Show, Eq, Generic)

data FontDimensionRef = FontDimensionRef HexInt FontRef
  deriving stock (Show, Eq, Generic)

data InternalLength
  = InternalLengthVariable (QuantVariableAST 'PT.LengthQuantity)
  | InternalSpecialLengthParameter PT.SpecialLengthParameter
  | InternalFontDimensionRef FontDimensionRef
  | InternalBoxDimensionRef BoxDimensionRef
  | LastKern
  deriving stock (Show, Eq, Generic)

data InternalGlue
  = InternalGlueVariable (QuantVariableAST 'PT.GlueQuantity)
  | LastGlue
  deriving stock (Show, Eq, Generic)

data InternalMathGlue
  = InternalMathGlueVariable (QuantVariableAST 'PT.MathGlueQuantity)
  | LastMathGlue
  deriving stock (Show, Eq, Generic)
