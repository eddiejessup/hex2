module Hex.Common.Quantity.MathLength where

import Formatting qualified as F
import Hex.Common.Quantity.Length (scaleIntByRational)
import Hex.Common.Quantity.Number
import Hexlude

newtype MathLength = MathLength {unMathLength :: HexInt}
  deriving stock (Show, Generic)
  deriving newtype (Eq, Ord)
  deriving (Semigroup, Monoid, Group) via (Sum Int)
  deriving (Scalable) via (HexInt)

-- Verified empirically, by observing that the smallest usable 'muskip' is
-- ~2^-16 mu.
muLength :: MathLength
muLength = MathLength $ HexInt (2 ^ (16 :: Int))

scaleMathLengthByRational :: Rational -> MathLength -> MathLength
scaleMathLengthByRational d p =
  MathLength $ scaleIntByRational d p.unMathLength

zeroMathLength :: MathLength
zeroMathLength = mempty

scaleMathLength :: HexInt -> MathLength -> MathLength
scaleMathLength = scale

shrinkMathLength :: HexInt -> MathLength -> MathLength
shrinkMathLength = shrink

fmtMathLengthMagnitude :: Fmt MathLength
fmtMathLengthMagnitude = fmtViewed #unMathLength F.shown

fmtMathLengthWithUnit :: Fmt MathLength
fmtMathLengthWithUnit = fmtMathLengthMagnitude |%| "mu"
