module Hex.Stage.Build.AnyDirection.Breaking.Badness where

import Formatting qualified as F
import Hex.Common.Quantity (fmtLengthWithUnit)
import Hex.Common.Quantity qualified as Q
import Hex.Stage.Build.AnyDirection.Breaking.Types (BreakItem, breakPenalty)
import Hexlude

-- If a box has a size specification TEX will stretch or shrink glue in the box.
-- For glue with only finite stretch or shrink components the badness (see
-- Chapter 19) of stretching or shrinking is computed. In TEX version 3 the
-- badness of the box most recently constructed is available for inspection by
-- the user through the \badness parameter. Values for badness range 0–10 000,
-- but if the box is overfull it is 1 000 000.
-- - Tex By Topic, p65.

newtype FiniteBadnessVal = FiniteBadnessVal {unFiniteBadnessVal :: Q.HexInt}
  deriving stock (Show, Generic)
  deriving newtype (Eq, Ord)

zeroFiniteBadness :: FiniteBadnessVal
zeroFiniteBadness = minBound

maxFiniteBadness :: FiniteBadnessVal
maxFiniteBadness = maxBound

finiteBadness :: Rational -> FiniteBadnessVal
finiteBadness r = min maxBound (FiniteBadnessVal $ Q.HexInt $ round $ (abs r ^ (3 :: Int)) * 100)

instance Bounded FiniteBadnessVal where
  minBound = FiniteBadnessVal Q.zeroInt

  maxBound = FiniteBadnessVal $ Q.HexInt Q.tenK

data Badness = FiniteBadness FiniteBadnessVal | InfiniteBadness
  deriving stock (Show, Generic)

zeroBadness :: Badness
zeroBadness = FiniteBadness zeroFiniteBadness

breakIsAcceptable ::
  Q.HexInt ->
  BreakItem ->
  Badness ->
  Bool
breakIsAcceptable tolerance br = \case
  InfiniteBadness ->
    False
  FiniteBadness b ->
    breakPenalty br < Q.tenK && b.unFiniteBadnessVal <= tolerance

-- >>> finiteFlexBadness (Q.pt 1) (Q.pt 1)
-- Badness_ {unFiniteBadnessVal = 100}
-- >>> finiteFlexBadness (Q.pt 1) (Q.pt 2)
-- Badness_ {unFiniteBadnessVal = 12}
-- >>> finiteFlexBadness (Q.pt 1) (Q.pt 0)
-- Badness_ {unFiniteBadnessVal = 10000}

data GlueFlexProblem = GlueFlexProblem
  { flexInDirection :: Q.FlexInDirection,
    excessLength :: Q.Length
  }
  deriving stock (Show, Generic)

fmtGlueFlexProblem :: Fmt GlueFlexProblem
fmtGlueFlexProblem =
  ("GlueFlexProblem(excess=" |%| F.accessed (.excessLength) fmtLengthWithUnit)
    <> (", flex=" |%| F.accessed (.flexInDirection) Q.fmtFlexInDirection |%| ")")

glueFlexProblemIsOverfull :: GlueFlexProblem -> Bool
glueFlexProblemIsOverfull glueFlexProblem =
  case glueFlexProblem.flexInDirection.flexDirection of
    Q.Stretch -> False
    Q.Shrink ->
      case glueFlexProblem.flexInDirection.flexAmount of
        Q.InfPureFlex _ -> False
        Q.FinitePureFlex totalShrink ->
          totalShrink < glueFlexProblem.excessLength

glueFlexProblemBadness :: GlueFlexProblem -> Badness
glueFlexProblemBadness glueFlexProblem
  | glueFlexProblemIsOverfull glueFlexProblem = InfiniteBadness
  | otherwise = case glueFlexProblem.flexInDirection.flexAmount of
      Q.InfPureFlex _ ->
        zeroBadness
      Q.FinitePureFlex netFiniteFlex
        | netFiniteFlex == Q.zeroLength -> FiniteBadness maxFiniteBadness
        | otherwise ->
            FiniteBadness $
              finiteBadness $
                Q.lengthRatio glueFlexProblem.excessLength netFiniteFlex
