module Hex.Stage.Build.Horizontal.Paragraph.Demerit where

import Hex.Common.Quantity qualified as Q
import Hex.Stage.Build.AnyDirection.Breaking.Badness
import Hex.Stage.Build.AnyDirection.Breaking.Types qualified as Breaking
import Hexlude

newtype Demerit = Demerit {unDemerit :: Int}
  deriving stock (Show, Generic)
  deriving newtype (Eq, Ord)

zeroDemerit :: Demerit
zeroDemerit = Demerit 0

combineDemerits :: Demerit -> Demerit -> Demerit
combineDemerits (Demerit a) (Demerit b) = Demerit (a + b)

demerit :: Q.HexInt -> Badness -> Breaking.BreakItem -> Demerit
demerit linePenalty badness breakItem =
  case badness of
    -- I made this up because it isn't mentioned.
    InfiniteBadness -> Demerit 100_000_000
    FiniteBadness finiteBadnessVal ->
      let breakItemPenalty = Breaking.breakPenalty breakItem

          pSq = breakItemPenalty ^ (2 :: Int)

          breakDemerit =
            if
                | (0 <= breakItemPenalty) && (breakItemPenalty < Q.tenK) -> pSq
                | (-Q.tenK < breakItemPenalty) && (breakItemPenalty < 0) -> -pSq
                | breakItemPenalty <= -Q.tenK -> 0
                | otherwise -> panic $ show breakItemPenalty

          listDemerit = (linePenalty <> finiteBadnessVal.unFiniteBadnessVal).unHexInt ^ (2 :: Int)
       in Demerit $ breakDemerit + listDemerit
