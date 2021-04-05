{-# LANGUAGE UndecidableInstances #-}

module Hex.Evaluate.AST.Common where

import Hex.Quantity qualified as H.Q
import Hex.Symbol.Token.Primitive qualified as H.Sym.Tok
import Hexlude

data Signed a = Signed [H.Q.Sign] a
  deriving stock (Show, Eq, Generic)

newtype HexInt = HexInt {unInt :: Signed UnsignedInt}
  deriving stock (Show, Eq, Generic)

data UnsignedInt
  = NormalUnsignedInt NormalInt
  | CoercedUnsignedInt CoercedInt
  deriving stock (Show, Eq, Generic)

data IntBase = Base8 | Base10 | Base16
  deriving stock (Show, Eq, Generic)

data IntConstantDigits = IntConstantDigits IntBase [Word8]
  deriving stock (Show, Eq, Generic)

-- Think: 'un-coerced integer'.
data NormalInt = IntConstant IntConstantDigits | CharLikeCode Word8 | InternalInt InternalInt
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
type Length = Signed UnsignedLength

zeroLength :: Length
zeroLength =
  Signed [H.Q.Positive] $
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
  = PhysicalUnit PhysicalUnitFrame H.Q.PhysicalUnit
  | InternalUnit InternalUnit
  deriving stock (Show, Eq, Generic)

scaledPointUnit :: Unit
scaledPointUnit = PhysicalUnit MagnifiedFrame H.Q.ScaledPoint

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

data ExplicitGlueSpec = ExplicitGlueSpec {egLength :: Length, egStretch :: Maybe Flex, egShrink :: Maybe Flex}
  deriving stock (Show, Eq, Generic)

data Flex = FiniteFlex Length | FilFlex FilLength
  deriving stock (Show, Eq, Generic)

oneFilFlex, minusOneFilFlex, oneFillFlex :: Flex
oneFilFlex = FilFlex oneFil
minusOneFilFlex = FilFlex minusOneFil
oneFillFlex = FilFlex oneFill

data FilLength = FilLength (Signed Factor) H.Q.InfLengthOrder
  deriving stock (Show, Eq, Generic)

oneFil, minusOneFil, oneFill :: FilLength
oneFil = FilLength (Signed [H.Q.Positive] oneFactor) H.Q.Fil1
minusOneFil = FilLength (Signed [H.Q.Negative] oneFactor) H.Q.Fil1
oneFill = FilLength (Signed [H.Q.Positive] oneFactor) H.Q.Fil2

-- Math glue.
data MathGlue
  = ExplicitMathGlue MathLength (Maybe MathFlex) (Maybe MathFlex)
  | InternalMathGlue (Signed InternalMathGlue)
  deriving stock (Show, Eq, Generic)

data MathFlex = FiniteMathFlex MathLength | FilMathFlex FilLength
  deriving stock (Show, Eq, Generic)

type family QuantParam (a :: H.Sym.Tok.QuantityType) where
  QuantParam 'H.Sym.Tok.IntQuantity = H.Sym.Tok.IntParameter
  QuantParam 'H.Sym.Tok.LenQuantity = H.Sym.Tok.LengthParameter
  QuantParam 'H.Sym.Tok.GlueQuantity = H.Sym.Tok.GlueParameter
  QuantParam 'H.Sym.Tok.MathGlueQuantity = H.Sym.Tok.MathGlueParameter
  QuantParam 'H.Sym.Tok.TokenListQuantity = H.Sym.Tok.TokenListParameter

-- Internal quantities.
data QuantVariableAST (a :: H.Sym.Tok.QuantityType) = ParamVar (QuantParam a) | RegisterVar RegisterLocation
  deriving stock (Generic)

deriving stock instance Show (QuantParam a) => Show (QuantVariableAST a)

deriving stock instance Eq (QuantParam a) => Eq (QuantVariableAST a)

data RegisterLocation = ExplicitRegisterLocation HexInt | InternalRegisterLocation H.Q.HexInt
  deriving stock (Show, Eq, Generic)

data InternalInt
  = InternalIntVariable (QuantVariableAST 'H.Sym.Tok.IntQuantity)
  | InternalSpecialIntParameter H.Sym.Tok.SpecialIntParameter
  | InternalCodeTableRef CodeTableRef
  | InternalCharToken H.Q.HexInt
  | InternalMathCharToken H.Q.HexInt
  | InternalFontCharRef FontCharRef
  | LastPenalty
  | ParShape
  | InputLineNr
  | Badness
  deriving stock (Show, Eq, Generic)

data CodeTableRef = CodeTableRef H.Sym.Tok.CodeType HexInt
  deriving stock (Show, Eq, Generic)

data FontCharRef = FontCharRef H.Sym.Tok.FontChar FontRef
  deriving stock (Show, Eq, Generic)

data FontRef
  = FontTokenRef H.Q.HexInt
  | CurrentFontRef
  | FamilyMemberFontRef FamilyMember
  deriving stock (Show, Eq, Generic)

data FamilyMember = FamilyMember H.Sym.Tok.FontRange HexInt
  deriving stock (Show, Eq, Generic)

data BoxDimensionRef = BoxDimensionRef HexInt H.Q.BoxDim
  deriving stock (Show, Eq, Generic)

data FontDimensionRef = FontDimensionRef HexInt FontRef
  deriving stock (Show, Eq, Generic)

data InternalLength
  = InternalLengthVariable (QuantVariableAST 'H.Sym.Tok.LenQuantity)
  | InternalSpecialLengthParameter H.Sym.Tok.SpecialLengthParameter
  | InternalFontDimensionRef FontDimensionRef
  | InternalBoxDimensionRef BoxDimensionRef
  | LastKern
  deriving stock (Show, Eq, Generic)

data InternalGlue = InternalGlueVariable (QuantVariableAST 'H.Sym.Tok.GlueQuantity) | LastGlue
  deriving stock (Show, Eq, Generic)

data InternalMathGlue
  = InternalMathGlueVariable (QuantVariableAST 'H.Sym.Tok.MathGlueQuantity)
  | LastMathGlue
  deriving stock (Show, Eq, Generic)
