module Hex.Common.Quantity.Length where

import Data.Fixed qualified as Fixed
import Data.Ratio qualified as Ratio
import Formatting qualified as F
import Hex.Common.Quantity.Number
import Hexlude

-- =========================================================================
-- The 'official' internal length quantity: integer number of scaled points.
-- =========================================================================

newtype Length = Length {unLength :: Int}
  deriving stock (Show, Generic)
  deriving newtype (Eq, Ord, Bounded)
  deriving (Semigroup, Monoid, Group) via (Sum Int)
  deriving (Scalable) via (HexInt)

lengthFromInt :: HexInt -> Length
lengthFromInt n = Length (n.unHexInt)

lengthAsInt :: Length -> HexInt
lengthAsInt len = HexInt (len.unLength)

-- | Scale a length by some integer. No rounding happens, ie no information is
-- lost (assuming no overflow).

-- | Scale an int by a rational number, rounding the result to the nearest
-- integer.
scaleIntByRational :: Rational -> Int -> Int
scaleIntByRational d p =
  let spRational = fromIntegral @Int @Rational p
   in round @Rational @Int (d * spRational)

-- | Scale a length by a rational number, rounding the result to the nearest
-- integer.
scaleLengthByRational :: Rational -> Length -> Length
scaleLengthByRational d p =
  Length $ scaleIntByRational d p.unLength

-- | Scale a length by a HexInt.
scaleLength :: HexInt -> Length -> Length
scaleLength = scale

-- | Shrink a length by a HexInt, ignoring the remainder.
shrinkLength :: HexInt -> Length -> Length
shrinkLength = shrink

zeroLength :: Length
zeroLength = mempty

-- Find the ratio between two lengths.
lengthRatio :: Length -> Length -> Rational
lengthRatio a b =
  let lenToInteger = view (typed @Int % to (fromIntegral @Int @Integer))
   in lenToInteger a Ratio.% lenToInteger b

-- Concepts.
------------

-- Physical units.
-- ---------------

onePt :: Length
onePt = pt 1

oneKPt :: Length
oneKPt = pt 1000

pt :: Rational -> Length
pt n = fromUnit n Point

mm :: Rational -> Length
mm n = fromUnit n Millimetre

inch :: Rational -> Length
inch n = fromUnit n Inch

fromUnit :: Rational -> PhysicalUnit -> Length
fromUnit n unit = scaleLengthByRational n (inScaledPoint unit)

-- 1 point is 2^16 scaled points. (65,536 scaled points)
pointLength :: Length
pointLength = Length (2 ^ (16 :: Int))

-- Functions related to units used in the TeX world.
-- A scaled point is defined as a fraction:
-- > num = 2.54 * 1e7 = 25400000
-- > den = 7227 * 2^16 = 473628672
-- > 1 scaled point = num / den = 5.4 nm
-- The DVI format's base unit is 100 nm.
-- Basic facts.
-- - 1 pica is 12 points.
-- - 1 inch is 72.27 points.
-- - 1 inch is 72 big points.
-- - 1 inch is 25.4 mm.
-- - 1 centimetre is 10 millimetres.
-- - 1 didot is 1238/1157 points.
-- - 1 cicero is 12 didots.

picaInPoint :: Int
picaInPoint = 12

ciceroInDidot :: Int
ciceroInDidot = 12

data PhysicalUnit
  = Point -- 'pt'
  | Pica -- 'pc'
  | Inch -- 'in'
  | BigPoint -- 'bp'
  | Centimetre -- 'cm'
  | Millimetre -- 'mm'
  | Didot -- 'dd'
  | Cicero -- 'cc'
  | ScaledPoint -- 'sp'
  deriving stock (Show, Eq, Generic)

inScaledPoint :: PhysicalUnit -> Length
inScaledPoint u = case u of
  Point -> pointLength
  Pica -> scaleLength (HexInt picaInPoint) pointLength
  -- 4,736,286.72 scaled points per inch.
  Inch -> Length 4_736_287
  -- 65,781.76 scaled points per big-point.
  BigPoint -> Length 65_782
  Centimetre -> scaleLength (HexInt 10) (inScaledPoint Millimetre)
  -- 2.8452755906 points per millimetre.
  -- 186,467.9811023622 scaled points per millimetre.
  Millimetre -> Length 186_468
  -- 70,124.0864304235 scaled points per didot
  Didot -> Length 70_124
  Cicero -> scaleLength (HexInt ciceroInDidot) (inScaledPoint Didot)
  ScaledPoint -> Length 1

-- Display.
-- --------

fmtRational :: Fmt Rational
fmtRational = F.later $ \r ->
  F.bformat F.shortest (fromRational @Fixed.Centi $ r)

fmtLengthMagnitude :: Fmt Length
fmtLengthMagnitude = F.accessed lengthInPoints fmtRational
  where
    lengthInPoints :: Length -> Rational
    lengthInPoints len = lengthRatio len pointLength

fmtLengthWithUnit :: Fmt Length
fmtLengthWithUnit = fmtLengthMagnitude |%| "pt"
