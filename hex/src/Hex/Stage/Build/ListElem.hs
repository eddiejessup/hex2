module Hex.Stage.Build.ListElem where

import Formatting qualified as F
import Hex.Common.Box qualified as Box
import Hex.Common.Quantity qualified as Q
import Hex.Stage.Build.AnyDirection.Breaking.Badness (FiniteBadnessVal)
import Hex.Stage.Build.BoxElem qualified as BoxElem
import Hexlude

-- Elements.

-- Vertical
data VListElem
  = VListBaseElem BoxElem.BaseElem
  | ListGlue Q.Glue
  | ListPenalty FiniteBadnessVal
  deriving stock (Show, Generic)

vListElemIsBox :: VListElem -> Bool
vListElemIsBox = \case
  VListBaseElem baseElem -> case baseElem of
    BoxElem.ElemBox _ -> True
    BoxElem.ElemFontDefinition _ -> False
    BoxElem.ElemFontSelection _ -> False
    BoxElem.ElemKern _ -> False
  ListGlue _ -> False
  ListPenalty _ -> False

newtype Penalty = Penalty {unPenalty :: Q.HexInt}
  deriving stock (Show, Eq, Ord, Generic)
  deriving newtype (Semigroup, Monoid, Group)

fmtPenalty :: Fmt Penalty
fmtPenalty = F.accessed (.unPenalty) ("P" |%| Q.fmtHexIntSimple)

zeroPenalty :: Penalty
zeroPenalty = Penalty Q.zeroInt

-- Horizontal
-- TODO: WhatsIt, Leaders, Mark, Insertion
-- TODO: Ligature, Math on/off, V-adust
data HListElem
  = HVListElem VListElem
  | HListHBaseElem BoxElem.HBaseElem
  | DiscretionaryItemElem DiscretionaryItem
  deriving stock (Show, Generic)

hListElemNaturalWidth :: HListElem -> Q.Length
hListElemNaturalWidth = \case
  HVListElem vListElem -> case vListElem of
    VListBaseElem baseElem -> case baseElem of
      BoxElem.ElemBox box ->
        box.unBaseBox.boxWidth
      BoxElem.ElemFontDefinition _fontDefinition ->
        Q.zeroLength
      BoxElem.ElemFontSelection _fontSelection ->
        Q.zeroLength
      BoxElem.ElemKern kern ->
        kern.unKern
    ListGlue glue ->
      glue.gDimen
    ListPenalty _penalty -> Q.zeroLength
  HListHBaseElem hBaseElem -> case hBaseElem of
    BoxElem.ElemCharacter charBox ->
      charBox.unCharacter.boxWidth
  DiscretionaryItemElem discrItem ->
    BoxElem.hBoxNaturalWidth discrItem.noBreakText

hListElemNaturalDepth :: HListElem -> Q.Length
hListElemNaturalDepth = \case
  HVListElem vListElem -> case vListElem of
    VListBaseElem baseElem -> case baseElem of
      BoxElem.ElemBox box ->
        box.unBaseBox.boxDepth
      BoxElem.ElemFontDefinition _fontDefinition ->
        Q.zeroLength
      BoxElem.ElemFontSelection _fontSelection ->
        Q.zeroLength
      BoxElem.ElemKern _kern ->
        Q.zeroLength
    ListGlue _glue ->
      Q.zeroLength
    ListPenalty _penalty ->
      Q.zeroLength
  HListHBaseElem hBaseElem -> case hBaseElem of
    BoxElem.ElemCharacter charBox ->
      charBox.unCharacter.boxDepth
  DiscretionaryItemElem discrItem ->
    BoxElem.hBoxNaturalDepth discrItem.noBreakText

hListElemNaturalHeight :: HListElem -> Q.Length
hListElemNaturalHeight = \case
  HVListElem vListElem -> case vListElem of
    VListBaseElem baseElem -> case baseElem of
      BoxElem.ElemBox box ->
        box.unBaseBox.boxHeight
      BoxElem.ElemFontDefinition _fontDefinition ->
        Q.zeroLength
      BoxElem.ElemFontSelection _fontSelection ->
        Q.zeroLength
      BoxElem.ElemKern _kern ->
        Q.zeroLength
    ListGlue _glue ->
      Q.zeroLength
    ListPenalty _penalty ->
      Q.zeroLength
  HListHBaseElem hBaseElem -> case hBaseElem of
    BoxElem.ElemCharacter charBox ->
      charBox.unCharacter.boxHeight
  DiscretionaryItemElem discrItem ->
    BoxElem.hBoxNaturalHeight discrItem.noBreakText

data DiscretionaryItem = DiscretionaryItem
  { preBreakText,
    postBreakText,
    noBreakText ::
      BoxElem.HBoxElemSeq
  }
  deriving stock (Show, Generic)

data DiscretionaryTextPart
  = DiscretionaryTextPartCharacter Box.CharBox
  | DiscretionaryTextBaseELem
  deriving stock (Show, Generic)

discretionaryHyphenItem :: Box.CharBox -> DiscretionaryItem
discretionaryHyphenItem c =
  DiscretionaryItem
    (BoxElem.singletonHBoxElemSeq $ BoxElem.HBoxHBaseElem $ BoxElem.ElemCharacter c)
    mempty
    mempty

discretionaryItemPenalty :: FiniteBadnessVal -> FiniteBadnessVal -> DiscretionaryItem -> FiniteBadnessVal
discretionaryItemPenalty hyphenPenalty explicitHyphenPenalty item = case item.preBreakText of
  BoxElem.HBoxElemSeq Empty -> explicitHyphenPenalty
  _ -> hyphenPenalty

hBoxElemSeqAsHListElems :: BoxElem.HBoxElemSeq -> Seq HListElem
hBoxElemSeqAsHListElems boxElemSeq =
  boxElemSeq.unHBoxElemSeq <&> \case
    BoxElem.HVBoxElem (BoxElem.VBoxBaseElem baseElem) ->
      HVListElem $ VListBaseElem baseElem
    BoxElem.HBoxHBaseElem hBaseElem ->
      HListHBaseElem hBaseElem

fmtDiscretionaryItem :: Fmt DiscretionaryItem
fmtDiscretionaryItem =
  "Discretionary{pre="
    |%| F.accessed (.preBreakText) BoxElem.fmtHBoxElemSeq
    <> ", post="
    |%| F.accessed (.postBreakText) BoxElem.fmtHBoxElemSeq
    <> ", full="
    |%| F.accessed (.noBreakText) BoxElem.fmtHBoxElemSeq

-- Lists.

newtype HList = HList {unHList :: Seq HListElem}
  deriving stock (Show, Generic)
  deriving newtype (Semigroup, Monoid)

hListElemTraversal :: Traversal' HList HListElem
hListElemTraversal = #unHList % traversed

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
  HListHBaseElem e -> bformat BoxElem.fmtHBaseElem e
  DiscretionaryItemElem discrItem ->
    bformat fmtDiscretionaryItem discrItem

newtype VList = VList {unVList :: Seq VListElem}
  deriving stock (Show, Generic)
  deriving newtype (Semigroup, Monoid)

vListElemTraversal :: Traversal' VList VListElem
vListElemTraversal = #unVList % traversed

vListNaturalHeight :: VList -> Q.Length
vListNaturalHeight = foldMapOf vListElemTraversal vListElemNaturalHeight

vListElemNaturalHeight :: VListElem -> Q.Length
vListElemNaturalHeight = \case
  ListGlue glue ->
    glue.gDimen
  ListPenalty _ ->
    Q.zeroLength
  VListBaseElem (BoxElem.ElemBox b) ->
    b.unBaseBox.boxHeight
  VListBaseElem (BoxElem.ElemFontDefinition _) ->
    Q.zeroLength
  VListBaseElem (BoxElem.ElemFontSelection _) ->
    Q.zeroLength
  VListBaseElem (BoxElem.ElemKern kern) ->
    kern.unKern

vListNetBiFlex :: VList -> Q.BiNetFlex
vListNetBiFlex = foldOf (vListElemTraversal % vListElemBiFlex)
  where
    vListElemBiFlex :: AffineFold VListElem Q.BiNetFlex
    vListElemBiFlex = _Typed @Q.Glue % to Q.asBiNetFlex

vListContainsBoxes :: VList -> Bool
vListContainsBoxes = anyOf vListElemTraversal vListElemIsBox

fmtVList :: Fmt VList
fmtVList = F.prefixed "\\vlist\n=====\n" $ fmtViewed #unVList fmtVListElemSeq

fmtVListElemSeq :: F.Format r (Seq VListElem -> r)
fmtVListElemSeq = F.intercalated "\n\n" fmtVListElem

fmtVListElem :: F.Format r (VListElem -> r)
fmtVListElem = F.later $ \case
  VListBaseElem e ->
    bformat BoxElem.fmtBaseElemOneLine e
  ListGlue g ->
    bformat Q.fmtGlue g
  ListPenalty _ ->
    "penalty"
