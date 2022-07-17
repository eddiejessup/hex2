{-# LANGUAGE UndecidableInstances #-}

module Hex.Stage.Parse.Interface.AST.Quantity where

import Hex.Common.Box qualified as Box
import Hex.Common.Codes qualified as Code
import Hex.Common.DVI.DocInstruction qualified as DVI
import Hex.Common.HexState.Interface.Font qualified as HSt.Font
import Hex.Common.HexState.Interface.Parameter qualified as HSt.Param
import Hex.Common.HexState.Interface.Register qualified as HSt.Reg
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

newtype MathLength = MathLength {unMathLength :: Signed UnsignedMathLength}
  deriving stock (Show, Eq, Generic)

data UnsignedMathLength
  = NormalMathLengthAsUMathLength NormalMathLength
  | CoercedMathLength CoercedMathLength
  deriving stock (Show, Eq, Generic)

-- Think: 'un-coerced length'.
data NormalMathLength
  = -- 'semi-constant' because Factor and Unit can be quite un-constant-like.
    MathLengthSemiConstant Factor MathUnit
  deriving stock (Show, Eq, Generic)

data MathUnit
  = Mu
  | InternalMathGlueAsUnit InternalMathGlue
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
  = ExplicitMathGlue MathLength (Maybe PureMathFlex) (Maybe PureMathFlex)
  | InternalMathGlue (Signed InternalMathGlue)
  deriving stock (Show, Eq, Generic)

data PureMathFlex
  = FinitePureMathFlex MathLength
  | InfPureMathFlex InfFlexOfOrder
  deriving stock (Show, Eq, Generic)

-- Internal quantities.
data QuantVariableAST (q :: Q.QuantityType)
  = ParamVar (HSt.Param.QuantParam q)
  | RegisterVar (QuantRegisterLocation q)
  deriving stock (Generic)

deriving stock instance Show (HSt.Param.QuantParam q) => Show (QuantVariableAST q)

deriving stock instance Eq (HSt.Param.QuantParam q) => Eq (QuantVariableAST q)

data QuantRegisterLocation (q :: Q.QuantityType)
  = ExplicitQuantRegisterLocation (HSt.Reg.QuantRegisterType q) ExplicitRegisterLocation
  | InternalQuantRegisterLocation (HSt.Reg.QuantRegisterLocation q)
  deriving stock (Show, Eq, Generic)

newtype ExplicitRegisterLocation = ExplicitRegisterLocation {unExplicitRegisterLocation :: HexInt}
  deriving stock (Show, Eq, Generic)

data InternalInt
  = InternalIntVariable (QuantVariableAST 'Q.IntQuantity)
  | InternalSpecialIntParameter HSt.Param.SpecialIntParameter
  | InternalCodeTableRef CodeTableRef
  | InternalCharToken Q.HexInt
  | InternalMathCharToken Q.HexInt
  | InternalFontSpecialCharRef FontSpecialCharRef
  | LastPenalty
  | ParShape
  | InputLineNr
  | Badness
  deriving stock (Show, Eq, Generic)

data CodeTableRef = CodeTableRef {codeType :: Code.CodeType, codeIndex :: CharCodeInt}
  deriving stock (Show, Eq, Generic)

-- | Newtype wrapper to represent HexInts that represent a char-code.
newtype CharCodeInt = CharCodeInt {unCharCodeInt :: HexInt}
  deriving stock (Show, Eq, Generic)

data FontSpecialCharRef = FontSpecialCharRef HSt.Font.FontSpecialChar FontRef
  deriving stock (Show, Eq, Generic)

data FontRef
  = FontTokenRef DVI.FontNumber
  | CurrentFontRef
  | FamilyMemberFontRef FamilyMember
  deriving stock (Show, Eq, Generic)

data FamilyMember = FamilyMember HSt.Font.FontRange HexInt
  deriving stock (Show, Eq, Generic)

data BoxDimensionRef = BoxDimensionRef ExplicitRegisterLocation Box.BoxDim
  deriving stock (Show, Eq, Generic)

data FontDimensionRef = FontDimensionRef HexInt FontRef
  deriving stock (Show, Eq, Generic)

data InternalLength
  = InternalLengthVariable (QuantVariableAST 'Q.LengthQuantity)
  | InternalSpecialLengthParameter HSt.Param.SpecialLengthParameter
  | InternalFontDimensionRef FontDimensionRef
  | InternalBoxDimensionRef BoxDimensionRef
  | LastKern
  deriving stock (Show, Eq, Generic)

data InternalGlue
  = InternalGlueVariable (QuantVariableAST 'Q.GlueQuantity)
  | LastGlue
  deriving stock (Show, Eq, Generic)

data InternalMathGlue
  = InternalMathGlueVariable (QuantVariableAST 'Q.MathGlueQuantity)
  | LastMathGlue
  deriving stock (Show, Eq, Generic)
