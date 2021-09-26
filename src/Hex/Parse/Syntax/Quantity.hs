{-# LANGUAGE UndecidableInstances #-}

module Hex.Parse.Syntax.Quantity where

import Hex.Quantity qualified as H.Q
import Hexlude
import qualified Hex.Syntax.Common as H.Syn
import qualified Hex.Syntax.Quantity as H.Syn

type instance H.Syn.HexPassInt 'H.Syn.Parsed = HexInt
type instance H.Syn.HexPassLength 'H.Syn.Parsed = Length
type instance H.Syn.HexPassMathLength 'H.Syn.Parsed = MathLength
type instance H.Syn.HexPassGlue 'H.Syn.Parsed = Glue
type instance H.Syn.HexPassRegisterIndex 'H.Syn.Parsed = RegisterIndex

data Signed a = Signed [H.Q.Sign] a
  deriving stock (Generic)

newtype HexInt = HexInt {unInt :: Signed UnsignedInt}
  deriving stock (Generic)

data UnsignedInt
  = NormalUnsignedInt NormalInt
  | CoercedUnsignedInt CoercedInt
  deriving stock (Generic)

data IntBase = Base8 | Base10 | Base16
  deriving stock (Generic)

data IntConstantDigits = IntConstantDigits IntBase [Word8]
  deriving stock (Generic)

-- Think: 'un-coerced integer'.
data NormalInt = IntConstant IntConstantDigits | CharLikeCode Word8 | InternalInt (H.Syn.InternalInt 'H.Syn.Parsed)
  deriving stock (Generic)

zeroInt :: NormalInt
zeroInt = IntConstant $ IntConstantDigits Base10 [0]

oneInt :: NormalInt
oneInt = IntConstant $ IntConstantDigits Base10 [1]

data CoercedInt
  = InternalLengthAsInt (H.Syn.InternalLength 'H.Syn.Parsed)
  | InternalGlueAsInt (H.Syn.InternalGlue 'H.Syn.Parsed)
  deriving stock (Generic)

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
  deriving stock (Generic)

-- Think: 'un-coerced length'.
data NormalLength
  = -- 'semi-constant' because Factor and Unit can be quite un-constant-like.
    LengthSemiConstant Factor Unit
  | InternalLength (H.Syn.InternalLength 'H.Syn.Parsed)
  deriving stock (Generic)

data Factor
  = NormalIntFactor NormalInt
  | -- Badly named 'decimal constant' in the TeXbook. Granted, it is specified
    -- with decimal digits, but its main feature is that it can represent
    -- non-integers.
    DecimalFractionFactor DecimalFraction
  deriving stock (Generic)

zeroFactor, oneFactor :: Factor
zeroFactor = NormalIntFactor zeroInt
oneFactor = NormalIntFactor oneInt

data DecimalFraction = DecimalFraction {wholeDigits :: [Word8], fracDigits :: [Word8]}
  deriving stock (Generic)

data Unit
  = PhysicalUnit PhysicalUnitFrame H.Q.PhysicalUnit
  | InternalUnit InternalUnit
  deriving stock (Generic)

scaledPointUnit :: Unit
scaledPointUnit = PhysicalUnit MagnifiedFrame H.Q.ScaledPoint

data InternalUnit
  = Em
  | Ex
  | InternalIntUnit (H.Syn.InternalInt 'H.Syn.Parsed)
  | InternalLengthUnit (H.Syn.InternalLength 'H.Syn.Parsed)
  | InternalGlueUnit (H.Syn.InternalGlue 'H.Syn.Parsed)
  deriving stock (Generic)

data PhysicalUnitFrame = MagnifiedFrame | TrueFrame
  deriving stock (Generic)

newtype CoercedLength = InternalGlueAsLength (H.Syn.InternalGlue 'H.Syn.Parsed)
  deriving stock (Generic)

-- Math-length.
type MathLength = Signed UnsignedMathLength

data UnsignedMathLength
  = NormalMathLengthAsUMathLength NormalMathLength
  | CoercedMathLength CoercedMathLength
  deriving stock (Generic)

-- Think: 'un-coerced length'.
data NormalMathLength
  = -- 'semi-constant' because Factor and Unit can be quite un-constant-like.
    MathLengthSemiConstant Factor MathUnit
  deriving stock (Generic)

data MathUnit = Mu | InternalMathGlueAsUnit (H.Syn.InternalMathGlue 'H.Syn.Parsed)
  deriving stock (Generic)

newtype CoercedMathLength = InternalMathGlueAsMathLength (H.Syn.InternalMathGlue 'H.Syn.Parsed)
  deriving stock (Generic)

-- Glue.
data Glue
  = ExplicitGlue ExplicitGlueSpec
  | InternalGlue (Signed (H.Syn.InternalGlue 'H.Syn.Parsed))
  deriving stock (Generic)

data ExplicitGlueSpec = ExplicitGlueSpec {egLength :: Length, egStretch :: Maybe Flex, egShrink :: Maybe Flex}
  deriving stock (Generic)

data Flex = FiniteFlex Length | FilFlex FilLength
  deriving stock (Generic)

oneFilFlex, minusOneFilFlex, oneFillFlex :: Flex
oneFilFlex = FilFlex oneFil
minusOneFilFlex = FilFlex minusOneFil
oneFillFlex = FilFlex oneFill

data FilLength = FilLength (Signed Factor) H.Q.InfLengthOrder
  deriving stock (Generic)

oneFil, minusOneFil, oneFill :: FilLength
oneFil = FilLength (Signed [H.Q.Positive] oneFactor) H.Q.Fil1
minusOneFil = FilLength (Signed [H.Q.Negative] oneFactor) H.Q.Fil1
oneFill = FilLength (Signed [H.Q.Positive] oneFactor) H.Q.Fil2

-- Math glue.
data MathGlue
  = ExplicitMathGlue MathLength (Maybe MathFlex) (Maybe MathFlex)
  | InternalMathGlue (Signed (H.Syn.InternalMathGlue 'H.Syn.Parsed))
  deriving stock (Generic)

data MathFlex = FiniteMathFlex MathLength | FilMathFlex FilLength
  deriving stock (Generic)

-- Internal quantities.

data RegisterIndex = ExplicitRegisterIndex HexInt | InternalRegisterIndex H.Q.HexInt
  deriving stock (Generic)

