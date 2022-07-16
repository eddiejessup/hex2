module Hex.Stage.Build.Horizontal.Paragraph.Demerit where

import Hex.Common.Quantity qualified as Q
import Hex.Stage.Build.AnyDirection.Breaking.Badness
import Hex.Stage.Build.AnyDirection.Breaking.Types qualified as Breaking
import Hexlude

-- Demerit.

newtype Demerit = Demerit {unDemerit :: Int}
  deriving stock (Show, Generic)

demerit :: Int -> Badness -> Breaking.BreakItem -> Demerit
demerit linePenalty badness breakItem =
  case badness of
    -- I made this up because it isn't mentioned.
    InfiniteBadness -> Demerit 100_000_000
    FiniteBadness finiteBadness ->
      let breakItemPenalty = Breaking.breakPenalty breakItem

          pSq = breakItemPenalty ^ (2 :: Int)

          breakDemerit =
            if
                | (0 <= breakItemPenalty) && (breakItemPenalty < Q.tenK) -> pSq
                | (-Q.tenK < breakItemPenalty) && (breakItemPenalty < 0) -> -pSq
                | breakItemPenalty <= -Q.tenK -> 0
                | otherwise -> panic $ show breakItemPenalty

          listDemerit = (linePenalty + finiteBadness ^. typed @Int) ^ (2 :: Int)
       in Demerit $ breakDemerit + listDemerit
