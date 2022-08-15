module Hex.Stage.Build.Horizontal.Paragraph.Demerit where

import Hex.Common.Quantity qualified as Q
import Hex.Stage.Build.AnyDirection.Breaking.Badness
import Hex.Stage.Build.Horizontal.Paragraph.Types qualified as H.Break
import Hex.Stage.Build.ListElem qualified as ListElem
import Hexlude

newtype Demerit = Demerit {unDemerit :: Q.HexInt}
  deriving stock (Show, Generic)
  deriving newtype (Eq, Ord)

zeroDemerit :: Demerit
zeroDemerit = Demerit Q.zeroInt

combineDemerits :: Demerit -> Demerit -> Demerit
combineDemerits (Demerit a) (Demerit b) = Demerit (a <> b)

demerit :: Q.HexInt -> ListElem.Penalty -> ListElem.Penalty -> Badness -> H.Break.HBreakItem -> Demerit
demerit linePenalty hyphenPenalty explicitHyphenPenalty badness breakItem =
  case badness of
    -- I made this up because it isn't mentioned.
    InfiniteBadness -> Demerit Q.maxInt
    FiniteBadness finiteBadnessVal ->
      let breakItemPenalty = H.Break.hBreakPenalty hyphenPenalty explicitHyphenPenalty breakItem

          pSq = Q.squareHexInt breakItemPenalty.unPenalty

          breakDemerit =
            if
                | (ListElem.Penalty Q.zeroInt <= breakItemPenalty) && (breakItemPenalty < ListElem.Penalty Q.tenKInt) -> pSq
                | (ListElem.Penalty (invert Q.tenKInt) < breakItemPenalty) && (breakItemPenalty < ListElem.Penalty Q.zeroInt) -> invert pSq
                | breakItemPenalty <= ListElem.Penalty (invert Q.tenKInt) -> Q.zeroInt
                | otherwise -> panic $ show breakItemPenalty

          listDemerit = Q.HexInt $ (linePenalty <> finiteBadnessVal.unFiniteBadnessVal).unHexInt ^ (2 :: Int)
       in Demerit $ breakDemerit <> listDemerit
