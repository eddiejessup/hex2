module Hex.Common.Quantity.Glue where

import Formatting qualified as F
import Hex.Common.Quantity.Length
import Hex.Common.Quantity.Number
import Hexlude

data Glue = Glue {gDimen :: Length, gStretch, gShrink :: PureFlex}
  deriving stock (Show, Eq, Generic)

fmtGlue :: Fmt Glue r
fmtGlue =
  "\\glue " F.% fmtViewed #gDimen fmtLengthWithUnit
    <> (" plus " F.% fmtViewed #gStretch fmtPureFlex)
    <> (" minus " F.% fmtViewed #gShrink fmtPureFlex)

zeroGlue :: Glue
zeroGlue = Glue zeroLength zeroFlex zeroFlex

invertGlue :: Glue -> Glue
invertGlue (Glue d str shr) = Glue (invert d) str shr

-- \fil
filStretchGlue :: Length -> Glue
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

data PureFlex = FinitePureFlex Length | InfPureFlex InfLengthOfOrder
  deriving stock (Show, Eq, Generic)

zeroFlex :: PureFlex
zeroFlex = FinitePureFlex zeroLength

flexPt :: Rational -> PureFlex
flexPt = FinitePureFlex . pt

filFlexPt :: Rational -> PureFlex
filFlexPt = filFlex . pt

filFlex :: Length -> PureFlex
filFlex x = InfPureFlex (InfLengthOfOrder Fil1 x)

scalePureFlex :: HexInt -> PureFlex -> PureFlex
scalePureFlex i = \case
  FinitePureFlex x -> FinitePureFlex $ scaleLength i x
  InfPureFlex x -> InfPureFlex $ scaleInfLengthOfOrder i x

shrinkPureFlex :: HexInt -> PureFlex -> PureFlex
shrinkPureFlex i = \case
  FinitePureFlex x -> FinitePureFlex $ shrinkLength i x
  InfPureFlex x -> InfPureFlex $ shrinkInfLengthOfOrder i x

fmtPureFlex :: Fmt PureFlex r
fmtPureFlex = F.later $ \case
  FinitePureFlex ln -> F.bformat fmtLengthWithUnit ln
  InfPureFlex ln -> F.bformat fmtInfLengthOfOrder ln

-- Net Flex.

data NetFlex = NetFlex {finiteFlex :: Length, fil1Flex, fil2Flex, fil3Flex :: Length}
  deriving stock (Show, Generic)

netFlexPt :: Rational -> NetFlex
netFlexPt = pureAsNetFlex . flexPt

filNetFlexPt :: Rational -> NetFlex
filNetFlexPt = pureAsNetFlex . filFlexPt

zeroNetFlex :: NetFlex
zeroNetFlex = NetFlex zeroLength zeroLength zeroLength zeroLength

instance Semigroup NetFlex where
  (NetFlex a1 b1 c1 d1) <> (NetFlex a2 b2 c2 d2) = NetFlex (a1 <> a2) (b1 <> b2) (c1 <> c2) (d1 <> d2)

instance Monoid NetFlex where
  mempty = zeroNetFlex

instance Group NetFlex where
  invert (NetFlex a1 b1 c1 d1) = NetFlex (invert a1) (invert b1) (invert c1) (invert d1)

pureAsNetFlex :: PureFlex -> NetFlex
pureAsNetFlex = \case
  FinitePureFlex len -> mempty {finiteFlex = len}
  InfPureFlex (InfLengthOfOrder order infLen) -> case order of
    Fil1 -> mempty {fil1Flex = infLen}
    Fil2 -> mempty {fil2Flex = infLen}
    Fil3 -> mempty {fil3Flex = infLen}

highestNetFlexOrder :: NetFlex -> PureFlex
highestNetFlexOrder flex
  | flex ^. #fil3Flex /= zeroLength = InfPureFlex (InfLengthOfOrder Fil3 (flex ^. #fil3Flex))
  | flex ^. #fil2Flex /= zeroLength = InfPureFlex (InfLengthOfOrder Fil2 (flex ^. #fil2Flex))
  | flex ^. #fil1Flex /= zeroLength = InfPureFlex (InfLengthOfOrder Fil1 (flex ^. #fil1Flex))
  | otherwise = FinitePureFlex (flex ^. #finiteFlex)

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

data InfLengthOrder = Fil1 | Fil2 | Fil3
  deriving stock (Show, Generic, Eq)

fmtInfLengthOrder :: Fmt InfLengthOrder r
fmtInfLengthOrder = F.later $ \case
  Fil1 -> "fil"
  Fil2 -> "fill"
  Fil3 -> "filll"

data InfLengthOfOrder = InfLengthOfOrder InfLengthOrder Length
  deriving stock (Show, Eq, Generic)

scaleInfLengthOfOrder :: HexInt -> InfLengthOfOrder -> InfLengthOfOrder
scaleInfLengthOfOrder i (InfLengthOfOrder order infLen) = InfLengthOfOrder order (scaleLength i infLen)

shrinkInfLengthOfOrder :: HexInt -> InfLengthOfOrder -> InfLengthOfOrder
shrinkInfLengthOfOrder i (InfLengthOfOrder order infLen) = InfLengthOfOrder order (shrinkLength i infLen)

fmtInfLengthOfOrder :: Fmt InfLengthOfOrder r
fmtInfLengthOfOrder = fmtViewed (typed @Length) fmtLengthMagnitude |<>| F.fconst " " |<>| fmtViewed (typed @InfLengthOrder) fmtInfLengthOrder
