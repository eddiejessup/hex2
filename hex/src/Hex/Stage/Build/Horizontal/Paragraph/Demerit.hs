module Hex.Stage.Build.Horizontal.Paragraph.Demerit where

import Hex.Common.Quantity qualified as Q
import Hex.Stage.Build.AnyDirection.Breaking.Badness
import Hex.Stage.Build.Horizontal.Paragraph.Types qualified as H.Break
import Hexlude

newtype Demerit = Demerit {unDemerit :: Q.HexInt}
  deriving stock (Show, Generic)
  deriving newtype (Eq, Ord)

zeroDemerit :: Demerit
zeroDemerit = Demerit Q.zeroInt

combineDemerits :: Demerit -> Demerit -> Demerit
combineDemerits (Demerit a) (Demerit b) = Demerit (a <> b)

demerit :: FiniteBadnessVal -> FiniteBadnessVal -> FiniteBadnessVal -> Badness -> H.Break.HBreakItem -> Demerit
demerit linePenalty hyphenPenalty explicitHyphenPenalty badness breakItem =
  case badness of
    -- I made this up because it isn't mentioned.
    InfiniteBadness -> Demerit Q.maxInt
    FiniteBadness finiteBadnessVal ->
      let breakItemPenalty = H.Break.hBreakPenalty hyphenPenalty explicitHyphenPenalty breakItem

          pSq = Q.squareHexInt breakItemPenalty.unFiniteBadnessVal

          breakDemerit =
            if
                | (zeroFiniteBadness <= breakItemPenalty) && (breakItemPenalty < FiniteBadnessVal Q.tenKInt) -> pSq
                | (FiniteBadnessVal (invert Q.tenKInt) < breakItemPenalty) && (breakItemPenalty < zeroFiniteBadness) -> invert pSq
                | breakItemPenalty <= FiniteBadnessVal (invert Q.tenKInt) -> Q.zeroInt
                | otherwise -> panic $ show breakItemPenalty

          listDemerit = Q.squareHexInt $ (linePenalty <> finiteBadnessVal).unFiniteBadnessVal
       in Demerit $ breakDemerit <> listDemerit
