module Hex.Quantity.MathLength where

import Hex.Quantity.Number
import Hexlude

newtype MathLength = MathLength {unMathLength :: Int}
  deriving stock (Show)
  deriving newtype (Num, Eq, Ord, Enum, Real, Integral)

scaleMathLength :: MathLength -> HexInt -> MathLength
scaleMathLength (MathLength d) (HexInt n) = MathLength (d * n)

shrinkMathLength :: MathLength -> HexInt -> MathLength
shrinkMathLength (MathLength d) (HexInt n) = MathLength (d `quot` n)
