module Hex.Stage.Build.Horizontal.Paragraph.Demerit where

import Hex.Common.Quantity qualified as Q
import Hex.Stage.Build.ListElem qualified as H.Inter.B.List
import Hex.Stage.Build.Horizontal.Badness
import Hex.Stage.Build.Horizontal.Paragraph.Types
import Hexlude

-- Demerit.

newtype Demerit = Demerit {unDemerit :: Int}
  deriving stock (Show, Generic)

demerit :: Int -> Badness -> BreakItem -> Demerit
demerit linePenalty badness_ breakItem =
  let breakItemPenalty = breakPenalty breakItem

      pSq = breakItemPenalty ^ (2 :: Int)

      breakDemerit =
        if
            | (0 <= breakItemPenalty) && (breakItemPenalty < Q.tenK) -> pSq
            | (-Q.tenK < breakItemPenalty) && (breakItemPenalty < 0) -> -pSq
            | breakItemPenalty <= -Q.tenK -> 0
            | otherwise -> panic $ show breakItemPenalty

      listDemerit = (linePenalty + badness_ ^. typed @Int) ^ (2 :: Int)
   in Demerit $ breakDemerit + listDemerit
  where
    breakPenalty :: BreakItem -> Int
    breakPenalty (PenaltyBreak (H.Inter.B.List.Penalty (Q.HexInt p))) = p
    breakPenalty (GlueBreak _) = 0
    breakPenalty (KernBreak _) = 0
