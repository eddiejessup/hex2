module Hex.Common.Quantity.Number where

import Data.Ratio qualified as Ratio
import Formatting qualified as F
import GHC.Num
import Hexlude

class Group a => Scalable a where
  scale :: HexInt -> a -> a

  shrink :: HexInt -> a -> a

newtype HexInt = HexInt {unHexInt :: Int}
  deriving stock (Show, Generic)
  deriving newtype (Eq, Ord, Enum)
  deriving (Semigroup, Monoid, Group) via (Sum Int)

maxInt :: HexInt
maxInt = HexInt $ 2 ^ (31 :: Int) - 1

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

squareHexInt :: HexInt -> HexInt
squareHexInt x = scaleHexInt x x

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

inThousands :: HexInt -> Rational
inThousands a = intRatio a thousandInt

fmtHexInt :: Fmt HexInt
fmtHexInt = "H." |%| fmtHexIntSimple

fmtHexIntSimple :: Fmt HexInt
fmtHexIntSimple = F.accessed (.unHexInt) F.shown

zeroInt :: HexInt
zeroInt = mempty

thousandInt :: HexInt
thousandInt = HexInt thousand

thousand :: Int
thousand = 1000

tenK :: Int
tenK = 10000

tenKInt :: HexInt
tenKInt = HexInt tenK

hunK :: Int
hunK = 100000

hunKInt :: HexInt
hunKInt = HexInt hunK

newNBitInt :: Alternative f => (Int -> a) -> Int -> Int -> f a
newNBitInt f nBits n
  | n < 0 = empty
  | n >= (2 ^ nBits) = empty
  | otherwise = pure $ f n

-- 4-bit.

newtype FourBitInt = FourBitInt Int
  deriving stock (Show, Generic)
  deriving newtype (Eq, Ord)

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
