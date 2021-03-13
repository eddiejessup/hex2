{-# LANGUAGE PatternSynonyms #-}
module Hex.Quantity.Length where

import Hexlude
import Hex.Quantity.Number

-- =========================================================================
-- The 'official' internal length quantity: integer number of scaled points.
-- =========================================================================

type Length = LengthScaledPoints Int

pattern Length :: Int -> Length
pattern Length x = LengthScaledPoints x

{-# COMPLETE Length #-}

lengthToInt :: Length -> HexInt
lengthToInt d = HexInt $ unLengthScaledPoints d

scaleLength :: Length -> HexInt -> Length
scaleLength (Length d) (HexInt n) = Length (d * n)

shrinkLength :: Length -> HexInt -> Length
shrinkLength (Length d) (HexInt n) = Length (d `quot` n)

oneKPt :: Length
oneKPt = lengthFromPointsInt 1000

-- Concepts.
------------

data HDirection
  = Leftward
  | Rightward
  deriving stock (Show, Eq, Generic)

data VDirection
  = Upward
  | Downward
  deriving stock (Show, Eq, Generic)

data Direction
  = Forward
  | Backward
  deriving stock (Show, Eq, Generic)

data Axis
  = Horizontal
  | Vertical
  deriving stock (Show, Eq, Generic)

data MoveMode
  = Put
  | Set
  deriving stock (Show)

data BoxDim
  = BoxWidth
  | BoxHeight
  | BoxDepth
  deriving stock (Show, Eq, Generic)

-- Physical units.
-- ---------------

-- -- Functions related to units used in the TeX world.
-- -- A scaled point is defined as a fraction:
-- -- > num = 2.54 * 1e7 = 25400000
-- -- > den = 7227 * 2^16 = 473628672
-- -- > 1 scaled point = num / den = 5.4 nm
-- -- The DVI format's base unit is 100 nm.
-- -- Basic facts.
-- -- 1 pica is 12 points.
-- picaInPoint :: Integer
-- picaInPoint = 12

-- -- 1 inch is 72.27 points.
-- inchInPoint :: Rational
-- inchInPoint = 7227 % 100

-- -- 1 inch is 72 big points.
-- inchInBigPoint :: Integer
-- inchInBigPoint = 72

-- -- 1 inch is 25.4 mm.
-- inchInMM :: Rational
-- inchInMM = 254 % 10

-- -- 1 centimetre is 10 millimetres.
-- cmInMM :: Integer
-- cmInMM = 10

-- -- 1 didot is 1238/1157 points.
-- didotInPoint :: Rational
-- didotInPoint = 1238 % 1157

-- -- 1 cicero is 12 didots.
-- ciceroInDidot :: Integer
-- ciceroInDidot = 12

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
  deriving stock (Show, Generic)

-- inScaledPoint :: PhysicalUnit -> Rational
-- inScaledPoint u = case u of
--     Point       -> fromIntegral pointInScaledPoint
--     Pica        -> fromIntegral $ picaInPoint * pointInScaledPoint
--     Inch        -> inchInPoint * inScaledPoint Point
--     BigPoint    -> inchInPoint * inScaledPoint Point / fromIntegral inchInBigPoint
--     Centimetre  -> 10 * inScaledPoint Millimetre
--     Millimetre  -> inScaledPoint Inch / inchInMM
--     Didot       -> didotInPoint * inScaledPoint Point
--     Cicero      -> 12 * inScaledPoint Didot
--     ScaledPoint -> 1

-- roundToDec :: RealFrac a => Int -> a -> a
-- roundToDec n v = fromInteger (round $ v * (10 ^ n)) / (10.0 ^^ n)

-- scaledPointIn :: PhysicalUnit -> Rational
-- scaledPointIn = recip . inScaledPoint

-- toScaledPoint :: Int -> PhysicalUnit -> Rational
-- toScaledPoint n u = realToFrac n * inScaledPoint u

-- toScaledPointApprox :: (Integral i) => Int -> PhysicalUnit -> i
-- toScaledPointApprox n u = round $ toScaledPoint n u

-- fromScaledPoint :: PhysicalUnit -> Rational
-- fromScaledPoint = recip . inScaledPoint

-- showFrac :: Real n => n -> Text
-- showFrac n = show $ roundToDec 1 (realToFrac n :: Double)

-- showSP :: Real n => n -> Text
-- showSP n =
--   showFrac ((realToFrac n * realToFrac (scaledPointIn Point)) :: Double) <> "pt"

-- Fonts.
---------

newtype LengthScaledPoints a = LengthScaledPoints {unLengthScaledPoints :: a}
  deriving stock (Show, Generic)
  deriving newtype (Eq, Ord, Enum)

deriving via (Sum a) instance Num a => Semigroup (LengthScaledPoints a)

deriving via (Sum a) instance Num a => Monoid (LengthScaledPoints a)

deriving via (Sum a) instance Num a => Group (LengthScaledPoints a)

newtype LengthDesignSize a = LengthDesignSize {unLengthDesignSize :: a}
  deriving stock (Show, Generic)
  deriving newtype (Eq, Ord)

deriving via (Sum a) instance Num a => Semigroup (LengthDesignSize a)

deriving via (Sum a) instance Num a => Monoid (LengthDesignSize a)

deriving via (Sum a) instance Num a => Group (LengthDesignSize a)

-- 1 point is 2^16 scaled points.
pointInScaledPoint :: Int
pointInScaledPoint = 2 ^ (16 :: Int)

lengthFromPointsInt :: Int -> LengthScaledPoints Int
lengthFromPointsInt v = LengthScaledPoints $ pointInScaledPoint * v

lengthFromPointsRational :: Rational -> LengthScaledPoints Rational
lengthFromPointsRational v = LengthScaledPoints $ fromIntegral @Int @Rational pointInScaledPoint * v

fromDesignSize :: LengthDesignSize Rational -> LengthScaledPoints Rational -> LengthScaledPoints Rational
fromDesignSize (LengthDesignSize d) (LengthScaledPoints p) = LengthScaledPoints (d * p)

roundScaledPoints :: LengthScaledPoints Rational -> LengthScaledPoints Int
roundScaledPoints (LengthScaledPoints p) = LengthScaledPoints $ round p
