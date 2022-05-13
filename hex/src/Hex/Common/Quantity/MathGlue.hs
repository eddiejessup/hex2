module Hex.Common.Quantity.MathGlue where

import Hex.Common.Quantity.Glue
import Hex.Common.Quantity.MathLength
import Hexlude
import Hex.Common.Quantity.Number

data MathGlue = MathGlue {mgDimen :: MathLength, mgStretch :: PureFlex, mgShrink :: PureFlex}
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
    <> (" plus " |%| fmtViewed #mgStretch fmtPureFlex)
    <> (" minus " |%| fmtViewed #mgShrink fmtPureFlex)

zeroMathGlue :: MathGlue
zeroMathGlue = MathGlue zeroMathLength zeroFlex zeroFlex

invertMathGlue :: MathGlue -> MathGlue
invertMathGlue (MathGlue d str shr) = MathGlue (invert d) str shr

scaleMathGlue :: HexInt -> MathGlue -> MathGlue
scaleMathGlue i (MathGlue dim str shr) =
  MathGlue (scaleMathLength i dim) (scalePureFlex i str) (scalePureFlex i shr)

shrinkMathGlue :: HexInt -> MathGlue -> MathGlue
shrinkMathGlue i (MathGlue dim str shr) =
  MathGlue (shrinkMathLength i dim) (shrinkPureFlex i str) (shrinkPureFlex i shr)
