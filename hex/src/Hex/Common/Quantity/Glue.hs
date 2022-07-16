module Hex.Common.Quantity.Glue where

import Data.Ratio qualified as Ratio
import Formatting qualified as F
import Hex.Common.Quantity.Length
import Hex.Common.Quantity.Number
import Hexlude

data Glue = Glue {gDimen :: Length, gStretch, gShrink :: PureFlex}
  deriving stock (Show, Eq, Generic)

glueAsLength :: Glue -> Length
glueAsLength g = g.gDimen

instance Semigroup Glue where
  -- Add the lengths, and
  (Glue dA strA shrA) <> (Glue dB strB shrB) =
    Glue (dA <> dB) (strA <> strB) (shrA <> shrB)

instance Monoid Glue where
  mempty = zeroGlue

instance Group Glue where
  invert = invertGlue

instance Scalable Glue where
  scale :: HexInt -> Glue -> Glue
  scale = scaleGlue

  shrink :: HexInt -> Glue -> Glue
  shrink = shrinkGlue

fmtGlue :: Fmt Glue
fmtGlue =
  "\\glue " |%| fmtViewed #gDimen fmtLengthWithUnit
    <> (" plus " |%| fmtViewed #gStretch fmtPureFlex)
    <> (" minus " |%| fmtViewed #gShrink fmtPureFlex)

zeroGlue :: Glue
zeroGlue = Glue zeroLength zeroFlex zeroFlex

invertGlue :: Glue -> Glue
invertGlue (Glue d str shr) = Glue (invert d) str shr

-- \fil
filStretchGlue :: InfLength -> Glue
filStretchGlue x = Glue {gDimen = mempty, gStretch = filFlex x, gShrink = zeroFlex}

fixedGlue :: Length -> Glue
fixedGlue d = Glue {gDimen = d, gStretch = zeroFlex, gShrink = zeroFlex}

-- Multiplication and division are possible too, but only by integers. For
-- example, ‘\multiply\dimen4 by 3’ triples the value of \dimen4, and
-- ‘\divide\skip5 by 2’ cuts in half all three components of the glue that is
-- currently registered in \skip5. You shouldn’t divide by zero, nor should you
-- multiply by numbers that will make the results exceed the register
-- capacities. Division of a positive integer by a positive integer sp discards
-- the remainder, and the sign of the result changes if you change the sign of
-- either operand. For example, 14 divided by 3 yields 4; −14 divided by 3
-- yields −4; number dimen glue −14 divided by −3 yields 4. Dimension values are
-- integer multiples of sp (scaled points).

scaleGlue :: HexInt -> Glue -> Glue
scaleGlue i (Glue dim str shr) =
  Glue (scaleLength i dim) (scalePureFlex i str) (scalePureFlex i shr)

-- \divide <glue> by 2’ halves all three components of <glue>.
shrinkGlue :: HexInt -> Glue -> Glue
shrinkGlue i (Glue dim str shr) =
  Glue (shrinkLength i dim) (shrinkPureFlex i str) (shrinkPureFlex i shr)

-- PureFlex

data PureFlex
  = FinitePureFlex Length
  | InfPureFlex InfFlexOfOrder
  deriving stock (Show, Eq, Generic)

instance Semigroup PureFlex where
  FinitePureFlex a <> FinitePureFlex b = FinitePureFlex (a <> b)
  FinitePureFlex _ <> InfPureFlex b = InfPureFlex b
  InfPureFlex a <> FinitePureFlex _ = InfPureFlex a
  InfPureFlex a <> InfPureFlex b = InfPureFlex (a <> b)

instance Monoid PureFlex where
  mempty = zeroFlex

zeroFlex :: PureFlex
zeroFlex = FinitePureFlex zeroLength

finFlex :: Length -> PureFlex
finFlex = FinitePureFlex

filFlex :: InfLength -> PureFlex
filFlex x = InfPureFlex (InfFlexOfOrder x Fil1)

scalePureFlex :: HexInt -> PureFlex -> PureFlex
scalePureFlex i = \case
  FinitePureFlex x -> FinitePureFlex $ scaleLength i x
  InfPureFlex x -> InfPureFlex $ scaleInfFlexOfOrder i x

shrinkPureFlex :: HexInt -> PureFlex -> PureFlex
shrinkPureFlex i = \case
  FinitePureFlex x -> FinitePureFlex $ shrinkLength i x
  InfPureFlex x -> InfPureFlex $ shrinkInfFlexOfOrder i x

fmtPureFlex :: Fmt PureFlex
fmtPureFlex = F.later $ \case
  FinitePureFlex ln -> F.bformat fmtLengthWithUnit ln
  InfPureFlex ln -> F.bformat fmtInfFlexOfOrder ln

-- Net Flex.

data NetFlex = NetFlex {finiteFlex :: Length, fil1Flex, fil2Flex, fil3Flex :: InfLength}
  deriving stock (Show, Generic)

zeroNetFlex :: NetFlex
zeroNetFlex = NetFlex zeroLength zeroInfLength zeroInfLength zeroInfLength

instance Semigroup NetFlex where
  (NetFlex a1 b1 c1 d1) <> (NetFlex a2 b2 c2 d2) = NetFlex (a1 <> a2) (b1 <> b2) (c1 <> c2) (d1 <> d2)

instance Monoid NetFlex where
  mempty = zeroNetFlex

instance Group NetFlex where
  invert (NetFlex a1 b1 c1 d1) = NetFlex (invert a1) (invert b1) (invert c1) (invert d1)

pureAsNetFlex :: PureFlex -> NetFlex
pureAsNetFlex = \case
  FinitePureFlex len -> mempty {finiteFlex = len}
  InfPureFlex (InfFlexOfOrder infLen order) -> case order of
    Fil1 -> mempty {fil1Flex = infLen}
    Fil2 -> mempty {fil2Flex = infLen}
    Fil3 -> mempty {fil3Flex = infLen}

highestNetFlexOrder :: NetFlex -> PureFlex
highestNetFlexOrder flex
  | flex.fil3Flex /= zeroInfLength = InfPureFlex (InfFlexOfOrder (flex.fil3Flex) Fil3)
  | flex.fil2Flex /= zeroInfLength = InfPureFlex (InfFlexOfOrder (flex.fil2Flex) Fil2)
  | flex.fil1Flex /= zeroInfLength = InfPureFlex (InfFlexOfOrder (flex.fil1Flex) Fil1)
  | otherwise = FinitePureFlex flex.finiteFlex

-- BiNetFlex.
data BiNetFlex = BiNetFlex {biStretch, biShrink :: NetFlex}
  deriving stock (Show, Generic)

instance Semigroup BiNetFlex where
  (BiNetFlex strA shrA) <> (BiNetFlex strB shrB) =
    BiNetFlex (strA <> strB) (shrA <> shrB)

instance Monoid BiNetFlex where
  mempty = BiNetFlex zeroNetFlex zeroNetFlex

asBiNetFlex :: Glue -> BiNetFlex
asBiNetFlex g = BiNetFlex (g ^. #gStretch % to pureAsNetFlex) (g ^. #gShrink % to pureAsNetFlex)

-- Inf length of specified order.

data InfFlexOrder = Fil1 | Fil2 | Fil3
  deriving stock (Show, Generic, Eq, Ord)

fmtInfFlexOrder :: Fmt InfFlexOrder
fmtInfFlexOrder = F.later $ \case
  Fil1 -> "fil"
  Fil2 -> "fill"
  Fil3 -> "filll"

newtype InfLength = InfLength {unInfLength :: HexInt}
  deriving stock (Show, Generic)
  deriving newtype (Eq, Ord)
  deriving (Semigroup, Monoid, Group) via (Sum Int)
  deriving (Scalable) via (HexInt)

zeroInfLength :: InfLength
zeroInfLength = mempty

-- You can use fractional multiples of infinity like ‘3.25fil’, as long as you
-- stick to fewer than 16,384 fil-units. Tex actually does its calculations with
-- integer multiples of 2^−16 fil (or fill or filll);
bigFilLength :: InfLength
bigFilLength = InfLength $ HexInt (2 ^ (16 :: Int))

scaleInfLengthByRational :: Rational -> InfLength -> InfLength
scaleInfLengthByRational factor infLen =
  InfLength $ scaleIntByRational factor infLen.unInfLength

fromBigFils :: Rational -> InfLength
fromBigFils factor =
  scaleInfLengthByRational factor bigFilLength

infLengthRatio :: InfLength -> InfLength -> Rational
infLengthRatio a b =
  let lenToInteger = fromIntegral @Int @Integer . (.unInfLength.unHexInt)
   in lenToInteger a Ratio.% lenToInteger b

fmtInfLengthMagnitude :: Fmt InfLength
fmtInfLengthMagnitude = F.accessed lengthInFils fmtRational
  where
    lengthInFils :: InfLength -> Rational
    lengthInFils filLen = infLengthRatio filLen bigFilLength

fmtInfLengthWithUnit :: Fmt InfLength
fmtInfLengthWithUnit = fmtInfLengthMagnitude |%| "filunits"

data InfFlexOfOrder = InfFlexOfOrder InfLength InfFlexOrder
  deriving stock (Show, Eq, Generic)

instance Semigroup InfFlexOfOrder where
  a@(InfFlexOfOrder da orderA) <> b@(InfFlexOfOrder db orderB) =
    case compare orderA orderB of
      LT -> b
      GT -> a
      EQ -> InfFlexOfOrder (da <> db) orderA

scaleInfFlexOfOrder :: HexInt -> InfFlexOfOrder -> InfFlexOfOrder
scaleInfFlexOfOrder i (InfFlexOfOrder infLen order) =
  InfFlexOfOrder (scale i infLen) order

shrinkInfFlexOfOrder :: HexInt -> InfFlexOfOrder -> InfFlexOfOrder
shrinkInfFlexOfOrder i (InfFlexOfOrder infLen order) =
  InfFlexOfOrder (shrink i infLen) order

fmtInfFlexOfOrder :: Fmt InfFlexOfOrder
fmtInfFlexOfOrder = fmtViewed (typed @InfLength) fmtInfLengthMagnitude <> F.fconst " " <> fmtViewed (typed @InfFlexOrder) fmtInfFlexOrder

-- Directed flex (stretch or shrink indicated.)

data FlexDirection
  = Stretch
  | Shrink
  deriving stock (Show, Generic)

data FlexInDirection = FlexInDirection {flexDirection :: FlexDirection, flexAmount :: PureFlex}
  deriving stock (Show, Generic)
