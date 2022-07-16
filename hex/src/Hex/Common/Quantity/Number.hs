module Hex.Common.Quantity.Number where

import Data.Ratio qualified as Ratio
import Formatting qualified as F
import Hex.Common.Quantity.Common
import Hexlude

class Group a => Scalable a where
  scale :: HexInt -> a -> a

  shrink :: HexInt -> a -> a

newtype HexInt = HexInt {unHexInt :: Int}
  deriving stock (Show, Generic)
  deriving newtype (Eq, Ord, Enum)
  deriving (Semigroup, Monoid, Group) via (Sum Int)

instance Scalable HexInt where
  scale :: HexInt -> HexInt -> HexInt
  scale = scaleHexInt

  -- Division of a positive integer by a positive integer
  -- discards the remainder, and the sign of the result
  -- changes if you change the sign of either operand.
  shrink :: HexInt -> HexInt -> HexInt
  shrink = shrinkHexInt

scaleInDirection :: Scalable a => VDirection -> HexInt -> a -> a
scaleInDirection = \case
  Upward -> scale
  Downward -> shrink

scaleHexInt :: HexInt -> HexInt -> HexInt
scaleHexInt arg v = HexInt (arg.unHexInt * v.unHexInt)

-- Division of a positive integer by a positive integer
-- discards the remainder, and the sign of the result
-- changes if you change the sign of either operand.
shrinkHexInt :: HexInt -> HexInt -> HexInt
shrinkHexInt arg v = HexInt (v.unHexInt `quot` arg.unHexInt)

-- Find the ratio between two ints.
intRatio :: HexInt -> HexInt -> Rational
intRatio a b =
  let intToInteger = view (typed @Int % to (fromIntegral @Int @Integer))
   in intToInteger a Ratio.% intToInteger b

fmtHexInt :: Fmt HexInt
fmtHexInt = "H." |%| fmtHexIntSimple

fmtHexIntSimple :: Fmt HexInt
fmtHexIntSimple = F.accessed (.unHexInt) F.shown

zeroInt :: HexInt
zeroInt = mempty

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

-- Signs.

data Sign
  = Positive
  | Negative
  deriving stock (Show, Eq, Generic)

-- mconcat to fold over a list of signs.
instance Semigroup Sign where
  a <> b = if a == b then Positive else Negative

instance Monoid Sign where
  mempty = Positive

data Signed a = Signed Sign a
  deriving stock (Functor, Generic)

deriving stock instance Show a => Show (Signed a)
