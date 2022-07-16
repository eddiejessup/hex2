module Hex.Stage.Build.ListElem where

import Formatting qualified as F
import Hex.Common.Quantity qualified as Q
import Hex.Stage.Build.BoxElem qualified as H.Inter.B.Box
import Hexlude

-- Elements.

-- Vertical list.
data VListElem
  = VListBaseElem H.Inter.B.Box.BaseElem
  | ListGlue Q.Glue
  | ListPenalty Penalty
  deriving stock (Show, Generic)

vListElemIsBox :: VListElem -> Bool
vListElemIsBox = \case
  VListBaseElem baseElem -> case baseElem of
    H.Inter.B.Box.ElemBox _ -> True
    H.Inter.B.Box.ElemFontDefinition _ -> False
    H.Inter.B.Box.ElemFontSelection _ -> False
    H.Inter.B.Box.ElemKern _ -> False
  ListGlue _ -> False
  ListPenalty _ -> False

newtype Penalty = Penalty {unPenalty :: Q.HexInt}
  deriving stock (Show, Eq, Generic)

-- Horizontal list.
-- TODO: WhatsIt, Leaders, Mark, Insertion
-- TODO: Ligature, DiscretionaryBreak, Math on/off, V-adust
data HListElem
  = HVListElem VListElem
  | HListHBaseElem H.Inter.B.Box.HBaseElem
  deriving stock (Show, Generic)

hListElemNaturalWidth :: HListElem -> Q.Length
hListElemNaturalWidth = \case
  HVListElem vListElem -> case vListElem of
    VListBaseElem baseElem -> case baseElem of
      H.Inter.B.Box.ElemBox box ->
        box.boxWidth
      H.Inter.B.Box.ElemFontDefinition _fontDefinition ->
        Q.zeroLength
      H.Inter.B.Box.ElemFontSelection _fontSelection ->
        Q.zeroLength
      H.Inter.B.Box.ElemKern kern ->
        kern.unKern
    ListGlue glue ->
      glue.gDimen
    ListPenalty _penalty -> Q.zeroLength
  HListHBaseElem hBaseElem -> case hBaseElem of
    H.Inter.B.Box.ElemCharacter charBox ->
      charBox.unCharacter.boxWidth

hListElemNaturalDepth :: HListElem -> Q.Length
hListElemNaturalDepth = \case
  HVListElem vListElem -> case vListElem of
    VListBaseElem baseElem -> case baseElem of
      H.Inter.B.Box.ElemBox box ->
        box.boxDepth
      H.Inter.B.Box.ElemFontDefinition _fontDefinition ->
        Q.zeroLength
      H.Inter.B.Box.ElemFontSelection _fontSelection ->
        Q.zeroLength
      H.Inter.B.Box.ElemKern _kern ->
        Q.zeroLength
    ListGlue _glue ->
      Q.zeroLength
    ListPenalty _penalty ->
      Q.zeroLength
  HListHBaseElem hBaseElem -> case hBaseElem of
    H.Inter.B.Box.ElemCharacter charBox ->
      charBox.unCharacter.boxDepth

hListElemNaturalHeight :: HListElem -> Q.Length
hListElemNaturalHeight = \case
  HVListElem vListElem -> case vListElem of
    VListBaseElem baseElem -> case baseElem of
      H.Inter.B.Box.ElemBox box ->
        box.boxHeight
      H.Inter.B.Box.ElemFontDefinition _fontDefinition ->
        Q.zeroLength
      H.Inter.B.Box.ElemFontSelection _fontSelection ->
        Q.zeroLength
      H.Inter.B.Box.ElemKern _kern ->
        Q.zeroLength
    ListGlue _glue ->
      Q.zeroLength
    ListPenalty _penalty ->
      Q.zeroLength
  HListHBaseElem hBaseElem -> case hBaseElem of
    H.Inter.B.Box.ElemCharacter charBox ->
      charBox.unCharacter.boxHeight

-- Lists.

newtype HList = HList {unHList :: Seq HListElem}
  deriving stock (Show, Generic)
  deriving newtype (Semigroup, Monoid)

hListElemTraversal :: Traversal' HList HListElem
hListElemTraversal = #unHList % traversed

hListNaturalDimens :: HList -> (Q.Length, Q.Length, Q.Length)
hListNaturalDimens hList = (hListNaturalWidth hList, hListNaturalHeight hList, hListNaturalDepth hList)

hListNaturalDepth :: HList -> Q.Length
hListNaturalDepth hList =
  -- The empty HList has zero depth.
  fromMaybe Q.zeroLength $
    maximumOf (hListElemTraversal % to hListElemNaturalDepth) hList

hListNaturalHeight :: HList -> Q.Length
hListNaturalHeight hList =
  -- The empty HList has zero height.
  fromMaybe Q.zeroLength $
    maximumOf (hListElemTraversal % to hListElemNaturalHeight) hList

hListNaturalWidth :: HList -> Q.Length
hListNaturalWidth =
  foldMapOf hListElemTraversal hListElemNaturalWidth

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
  deriving newtype (Semigroup, Monoid)

vListElemTraversal :: Traversal' VList VListElem
vListElemTraversal = #unVList % traversed

vListContainsBoxes :: VList -> Bool
vListContainsBoxes = anyOf vListElemTraversal vListElemIsBox

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
