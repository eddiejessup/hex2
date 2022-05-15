module Hex.Common.Quantity.MathGlue where

import Hex.Common.Quantity.Glue
import Hex.Common.Quantity.MathLength
import Hexlude
import Hex.Common.Quantity.Number
import qualified Formatting as F

data MathGlue = MathGlue {mgDimen :: MathLength, mgStretch :: PureMathFlex, mgShrink :: PureMathFlex}
  deriving stock (Show, Eq, Generic)

instance Semigroup MathGlue where
  MathGlue dA strA shrA <> MathGlue dB strB shrB =
    MathGlue (dA <> dB) (strA <> strB) (shrA <> shrB)

instance Monoid MathGlue where
  mempty = zeroMathGlue

instance Group MathGlue where
  invert = invertMathGlue

instance Scalable MathGlue where
  scale :: HexInt -> MathGlue -> MathGlue
  scale = scaleMathGlue

  shrink :: HexInt -> MathGlue -> MathGlue
  shrink = shrinkMathGlue

fmtMathGlue :: Fmt MathGlue
fmtMathGlue =
  "\\muglue " |%| fmtViewed #mgDimen fmtMathLengthWithUnit
    <> (" plus " |%| fmtViewed #mgStretch fmtPureMathFlex)
    <> (" minus " |%| fmtViewed #mgShrink fmtPureMathFlex)

zeroMathGlue :: MathGlue
zeroMathGlue = MathGlue zeroMathLength zeroMathFlex zeroMathFlex

invertMathGlue :: MathGlue -> MathGlue
invertMathGlue (MathGlue d str shr) = MathGlue (invert d) str shr

scaleMathGlue :: HexInt -> MathGlue -> MathGlue
scaleMathGlue i (MathGlue dim str shr) =
  MathGlue (scaleMathLength i dim) (scalePureMathFlex i str) (scalePureMathFlex i shr)

shrinkMathGlue :: HexInt -> MathGlue -> MathGlue
shrinkMathGlue i (MathGlue dim str shr) =
  MathGlue (shrinkMathLength i dim) (shrinkPureMathFlex i str) (shrinkPureMathFlex i shr)

data PureMathFlex
  = FinitePureMathFlex MathLength
  | InfPureMathFlex InfFlexOfOrder
  deriving stock (Show, Eq, Generic)

instance Semigroup PureMathFlex where
  FinitePureMathFlex a <> FinitePureMathFlex b = FinitePureMathFlex (a <> b)
  FinitePureMathFlex _ <> InfPureMathFlex b = InfPureMathFlex b
  InfPureMathFlex a <> FinitePureMathFlex _ = InfPureMathFlex a
  InfPureMathFlex a <> InfPureMathFlex b = InfPureMathFlex (a <> b)

instance Monoid PureMathFlex where
  mempty = zeroMathFlex

zeroMathFlex :: PureMathFlex
zeroMathFlex = FinitePureMathFlex zeroMathLength

scalePureMathFlex :: HexInt -> PureMathFlex -> PureMathFlex
scalePureMathFlex i = \case
  FinitePureMathFlex x -> FinitePureMathFlex $ scaleMathLength i x
  InfPureMathFlex x -> InfPureMathFlex $ scaleInfFlexOfOrder i x

shrinkPureMathFlex :: HexInt -> PureMathFlex -> PureMathFlex
shrinkPureMathFlex i = \case
  FinitePureMathFlex x -> FinitePureMathFlex $ shrinkMathLength i x
  InfPureMathFlex x -> InfPureMathFlex $ shrinkInfFlexOfOrder i x

fmtPureMathFlex :: Fmt PureMathFlex
fmtPureMathFlex = F.later $ \case
  FinitePureMathFlex ln -> F.bformat fmtMathLengthWithUnit ln
  InfPureMathFlex ln -> F.bformat fmtInfFlexOfOrder ln
