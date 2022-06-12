module Hex.Stage.Build.ListElem where

import Formatting qualified as F
import Hex.Stage.Build.BoxElem qualified as H.Inter.B.Box
import Hex.Common.Quantity qualified as Q
import Hexlude

-- Elements.

-- Vertical list.
data VListElem
  = VListBaseElem H.Inter.B.Box.BaseElem
  | ListGlue Q.Glue
  | ListPenalty Penalty
  deriving stock (Show, Generic)

newtype Penalty = Penalty {unPenalty :: Q.HexInt}
  deriving stock (Show, Eq, Generic)

-- Horizontal list.
-- TODO: WhatsIt, Leaders, Mark, Insertion
-- TODO: Ligature, DiscretionaryBreak, Math on/off, V-adust
data HListElem
  = HVListElem VListElem
  | HListHBaseElem H.Inter.B.Box.HBaseElem
  deriving stock (Show, Generic)

-- Lists.

newtype HList = HList {unHList :: Seq HListElem}
  deriving stock (Show, Generic)

hListElemTraversal :: Traversal' HList HListElem
hListElemTraversal = #unHList % traversed

fmtHListOneLine :: Fmt HList
fmtHListOneLine = F.prefixed "\\hlist" $ F.braced (fmtViewed #unHList fmtHListElemsOneLine)

fmtHListMultiLine :: Fmt HList
fmtHListMultiLine = F.prefixed "\\hlist\n=======\n" (fmtViewed #unHList (F.unlined fmtHListElem))

fmtHListElemsOneLine :: Fmt (Seq HListElem)
fmtHListElemsOneLine = F.commaSpaceSep fmtHListElem

fmtHListElem :: Fmt HListElem
fmtHListElem = F.later $ \case
  HVListElem vEl -> bformat fmtVListElem vEl
  HListHBaseElem e -> bformat H.Inter.B.Box.fmtHBaseElem e

newtype VList = VList {unVList :: Seq VListElem}
  deriving stock (Show, Generic)

vListElemTraversal :: Traversal' VList VListElem
vListElemTraversal = #unVList % traversed

fmtVList :: Fmt VList
fmtVList = F.prefixed "\\vlist\n=====\n" $ fmtViewed #unVList fmtVListElemSeq

fmtVListElemSeq :: F.Format r (Seq VListElem -> r)
fmtVListElemSeq = F.intercalated "\n\n" fmtVListElem

fmtVListElem :: F.Format r (VListElem -> r)
fmtVListElem = F.later $ \case
  VListBaseElem e ->
    bformat H.Inter.B.Box.fmtBaseElemOneLine e
  ListGlue g ->
    bformat Q.fmtGlue g
  ListPenalty _ ->
    "penalty"
