module Hex.Common.Quantity.MathLength where

import Hex.Common.Quantity.Number
import Hexlude
import qualified Formatting as F

newtype MathLength = MathLength {unMathLength :: Int}
  deriving stock (Show, Generic)
  deriving newtype (Eq, Ord)
  deriving (Semigroup, Monoid, Group) via (Sum Int)
  deriving (Scalable) via (HexInt)

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
