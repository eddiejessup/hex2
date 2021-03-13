module Hex.Quantity.Glue where

import Hex.Quantity.Length
import Hex.Quantity.Number
import Hexlude

data Glue = Glue {gDimen :: Length, gStretch :: Flex, gShrink :: Flex}
  deriving stock (Show, Eq, Generic)

negateGlue :: Glue -> Glue
negateGlue (Glue d str shr) = Glue (invert d) str shr

instance Semigroup Glue where
  (Glue dA strA shrA) <> (Glue dB strB shrB) =
    Glue (dA <> dB) (strA <> strB) (shrA <> shrB)

instance Monoid Glue where
  mempty = Glue mempty mempty mempty

scaleGlue :: Glue -> HexInt -> Glue
scaleGlue (Glue dim str shr) i =
  Glue (scaleLength dim i) (scaleFlex str i) (scaleFlex shr i)

-- \divide <glue> by 2’ halves all three components of <glue>.
shrinkGlue :: Glue -> HexInt -> Glue
shrinkGlue (Glue dim str shr) i =
  Glue (shrinkLength dim i) (shrinkFlex str i) (shrinkFlex shr i)

filGlue :: Glue
filGlue = Glue {gDimen = mempty, gStretch = filFlex, gShrink = noFlex}

fixedGlue :: Length -> Glue
fixedGlue d = Glue {gDimen = d, gStretch = noFlex, gShrink = noFlex}

-- Flex.
data Flex
  = FiniteFlex Length
  | InfiniteFlex InfFlex
  deriving stock (Show, Generic, Eq)

instance Semigroup Flex where
  (FiniteFlex a) <> (FiniteFlex b) = FiniteFlex (a <> b)
  (InfiniteFlex a) <> (InfiniteFlex b) = InfiniteFlex (a <> b)
  a@(InfiniteFlex _) <> (FiniteFlex _) = a
  (FiniteFlex _) <> b@(InfiniteFlex _) = b

instance Monoid Flex where
  mempty = FiniteFlex mempty

scaleFlex :: Flex -> HexInt -> Flex
scaleFlex (FiniteFlex l) i = FiniteFlex (scaleLength l i)
scaleFlex (InfiniteFlex a) i = InfiniteFlex (scaleInfFlex a)
  where
    scaleInfFlex InfFlex {infFlexFactor, infFlexOrder} =
      InfFlex {infFlexFactor = infFlexFactor * fromIntegral @Int @Rational (i ^. typed @Int), infFlexOrder}

shrinkFlex :: Flex -> HexInt -> Flex
shrinkFlex (FiniteFlex a) i = FiniteFlex (shrinkLength a i)
shrinkFlex (InfiniteFlex a) i = InfiniteFlex (shrinkInfFlex a)
  where
    shrinkInfFlex InfFlex {infFlexFactor, infFlexOrder} = InfFlex {infFlexFactor = infFlexFactor / fromIntegral @Int @Rational (i ^. typed @Int), infFlexOrder}

noFlex :: Flex
noFlex = mempty

filFlex :: Flex
filFlex = InfiniteFlex filInfFlex

-- InfFlex.
data InfFlex = InfFlex {infFlexFactor :: Rational, infFlexOrder :: Int}
  deriving stock (Show, Generic, Eq)

instance Semigroup InfFlex where
  a@InfFlex {infFlexFactor = aFac, infFlexOrder = aOrd} <> b@InfFlex {infFlexFactor = bFac, infFlexOrder = bOrd} =
    case compare aOrd bOrd of
      LT -> b
      GT -> a
      EQ -> InfFlex {infFlexFactor = aFac + bFac, infFlexOrder = aOrd}

instance Monoid InfFlex where
  mempty = InfFlex 0 0

filInfFlex :: InfFlex
filInfFlex = InfFlex 1 1

-- scaleMathGlue :: Glue MathLength -> HexInt -> Glue MathLength
-- scaleMathGlue (Glue d str shr) i =
--   Glue (scaleMathLength d i) (scaleFlex str i) (scaleFlex shr i)

-- shrinkMathGlue :: Glue MathLength -> HexInt -> Glue MathLength
-- shrinkMathGlue (Glue d str shr) i =
--   Glue (shrinkMathLength d i) (shrinkFlex str i) (shrinkFlex shr i)
