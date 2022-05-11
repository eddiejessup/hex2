module Hex.Common.Quantity.MathLength where

import Hex.Common.Quantity.Number
import Hexlude

newtype MathLength = MathLength {unMathLength :: Int}
  deriving stock (Show)
  deriving newtype (Eq, Ord)
  deriving (Semigroup, Monoid, Group) via (Sum Int)
  deriving (Scalable) via (HexInt)

zeroMathLength :: MathLength
zeroMathLength = mempty

scaleMathLength :: HexInt -> MathLength -> MathLength
scaleMathLength = scale

shrinkMathLength :: HexInt -> MathLength -> MathLength
shrinkMathLength = shrink
