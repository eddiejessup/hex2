module Hex.Interpret.Build.List.Horizontal.Paragraph.Demerit where

import Hex.Interpret.Build.List.Elem qualified as H.Inter.B.List
import Hex.Interpret.Build.List.Horizontal.Badness
import Hex.Interpret.Build.List.Horizontal.Paragraph.Types
import Hex.Quantity qualified as H.Q
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
            | (0 <= breakItemPenalty) && (breakItemPenalty < H.Q.tenK) -> pSq
            | (- H.Q.tenK < breakItemPenalty) && (breakItemPenalty < 0) -> - pSq
            | breakItemPenalty <= - H.Q.tenK -> 0
            | otherwise -> panic $ show breakItemPenalty

      listDemerit = (linePenalty + badness_ ^. typed @Int) ^ (2 :: Int)
   in Demerit $ breakDemerit + listDemerit
  where
    breakPenalty :: BreakItem -> Int
    breakPenalty (PenaltyBreak (H.Inter.B.List.Penalty p)) = p
    breakPenalty (GlueBreak _) = 0
    breakPenalty (KernBreak _) = 0
