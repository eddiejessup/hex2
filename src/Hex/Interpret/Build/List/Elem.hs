module Hex.Interpret.Build.List.Elem where

import Formatting qualified as F
import Hex.Interpret.Build.Box.Elem qualified as H.Inter.B.Box
import Hex.Quantity qualified as H.Q
import Hexlude

-- Elements.

-- Vertical list.
data VListElem
  = VListBaseElem H.Inter.B.Box.BaseElem
  | ListGlue H.Q.Glue
  | ListPenalty Penalty
  deriving stock (Generic)

newtype Penalty = Penalty {unPenalty :: H.Q.HexInt}
  deriving stock (Show, Eq, Generic)

-- Horizontal list.
-- TODO: WhatsIt, Leaders, Mark, Insertion
-- TODO: Ligature, DiscretionaryBreak, Math on/off, V-adust
data HListElem
  = HVListElem VListElem
  | HListHBaseElem H.Inter.B.Box.HBaseElem
  deriving stock (Generic)

-- Lists.

newtype HList = HList {unHList :: Seq HListElem}
  deriving stock (Generic)

hListElemTraversal :: Traversal' HList HListElem
hListElemTraversal = #unHList % traversed

fmtHListOneLine :: Fmt HList r
fmtHListOneLine = F.prefixed "\\hlist" $ F.braced (fmtViewed #unHList fmtHListElemsOneLine)

fmtHListMultiLine :: Fmt HList r
fmtHListMultiLine = F.prefixed "\\hlist\n=======\n" (fmtViewed #unHList (F.unlined fmtHListElem))

fmtHListElemsOneLine :: Fmt (Seq HListElem) r
fmtHListElemsOneLine = F.commaSpaceSep fmtHListElem

fmtHListElem :: Fmt HListElem r
fmtHListElem = F.later $ \case
  HVListElem vEl -> bformat fmtVListElem vEl
  HListHBaseElem e -> bformat H.Inter.B.Box.fmtHBaseElem e

newtype VList = VList {unVList :: Seq VListElem}
  deriving stock (Generic)

vListElemTraversal :: Traversal' VList VListElem
vListElemTraversal = #unVList % traversed

fmtVList :: Fmt VList r
fmtVList = F.prefixed "\\vlist\n=====\n" $ fmtViewed #unVList fmtVListElemSeq

fmtVListElemSeq :: F.Format r (Seq VListElem -> r)
fmtVListElemSeq = F.intercalated "\n\n" fmtVListElem

fmtVListElem :: F.Format r (VListElem -> r)
fmtVListElem = F.later $ \case
  VListBaseElem e ->
    bformat H.Inter.B.Box.fmtBaseElemOneLine e
  ListGlue g ->
    bformat H.Q.fmtGlue g
  ListPenalty _ ->
    "penalty"

-- Element constituents.

data VBoxAlignType
  = DefaultAlign -- \vbox
  | TopAlign -- \vtop
  deriving stock (Generic)

data DesiredLength = Natural | Spread H.Q.Length | To H.Q.Length
  deriving stock (Show)
