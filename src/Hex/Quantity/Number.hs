module Hex.Quantity.Number where

import Hexlude

newtype HexInt = HexInt {unInt :: Int}
  deriving stock (Show, Generic)
  deriving newtype (Eq, Ord, Enum, Bounded, Hashable, Num, Bits, FiniteBits)

zeroHexInt :: HexInt
zeroHexInt = HexInt 0

tenK :: Int
tenK = 10000

hunK :: Int
hunK = 100000

newNBitInt :: Alternative f => (Int -> a) -> Int -> Int -> f a
newNBitInt f nBits n
  | n < 0 = empty
  | n >= (2 ^ nBits) = empty
  | otherwise = pure $ f n

-- 8-bit.

newtype EightBitInt = EightBitInt Int
  deriving stock (Show, Generic)
  deriving newtype (Eq, Ord, Enum)

newEightBitInt :: Alternative f => Int -> f EightBitInt
newEightBitInt = newNBitInt EightBitInt 8

-- 4-bit.

newtype FourBitInt = FourBitInt Int
  deriving stock (Show, Generic)
  deriving newtype (Eq, Ord, Enum, Hashable)

newFourBitInt :: Alternative f => Int -> f FourBitInt
newFourBitInt = newNBitInt FourBitInt 4
