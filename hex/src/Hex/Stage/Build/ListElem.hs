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

vListElemNaturalWidth :: VListElem -> Q.Length
vListElemNaturalWidth = \case
  ListGlue _glue ->
    Q.zeroLength
  ListPenalty _ ->
    Q.zeroLength
  VListBaseElem (BoxElem.AxOrRuleBoxBaseElem b) ->
    b.boxedDims.boxWidth
  VListBaseElem (BoxElem.KernBaseElem _kern) ->
    Q.zeroLength

vListElemNaturalHeight :: VListElem -> Q.Length
vListElemNaturalHeight = \case
  ListGlue glue ->
    glue.gDimen
  ListPenalty _ ->
    Q.zeroLength
  VListBaseElem (BoxElem.AxOrRuleBoxBaseElem b) ->
    b.boxedDims.boxHeight
  VListBaseElem (BoxElem.KernBaseElem kern) ->
    kern.unKern

vListElemNaturalDepth :: VListElem -> Q.Length
vListElemNaturalDepth = \case
  ListGlue _glue ->
    Q.zeroLength
  ListPenalty _ ->
    Q.zeroLength
  VListBaseElem (BoxElem.AxOrRuleBoxBaseElem b) ->
    b.boxedDims.boxDepth
  VListBaseElem (BoxElem.KernBaseElem _kern) ->
    Q.zeroLength

vListElemIsBox :: VListElem -> Bool
vListElemIsBox = \case
  VListBaseElem baseElem -> case baseElem of
    BoxElem.AxOrRuleBoxBaseElem _ -> True
    BoxElem.KernBaseElem _ -> False
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
      BoxElem.AxOrRuleBoxBaseElem box ->
        box.boxedDims.boxWidth
      BoxElem.KernBaseElem kern ->
        kern.unKern
    ListGlue glue ->
      glue.gDimen
    ListPenalty _penalty -> Q.zeroLength
  HListHBaseElem hBaseElem -> case hBaseElem of
    BoxElem.CharBoxHBaseElem charBox ->
      charBox.boxedDims.boxWidth
  DiscretionaryItemElem discrItem ->
    BoxElem.hBoxNaturalWidth discrItem.noBreakText

hListElemNaturalDepth :: HListElem -> Q.Length
hListElemNaturalDepth = \case
  HVListElem vListElem -> case vListElem of
    VListBaseElem baseElem -> case baseElem of
      BoxElem.AxOrRuleBoxBaseElem box ->
        box.boxedDims.boxDepth
      BoxElem.KernBaseElem _kern ->
        Q.zeroLength
    ListGlue _glue ->
      Q.zeroLength
    ListPenalty _penalty ->
      Q.zeroLength
  HListHBaseElem hBaseElem -> case hBaseElem of
    BoxElem.CharBoxHBaseElem charBox ->
      charBox.boxedDims.boxDepth
  DiscretionaryItemElem discrItem ->
    BoxElem.hBoxNaturalDepth discrItem.noBreakText

hListElemNaturalHeight :: HListElem -> Q.Length
hListElemNaturalHeight = \case
  HVListElem vListElem -> case vListElem of
    VListBaseElem baseElem -> case baseElem of
      BoxElem.AxOrRuleBoxBaseElem box ->
        box.boxedDims.boxHeight
      BoxElem.KernBaseElem _kern ->
        Q.zeroLength
    ListGlue _glue ->
      Q.zeroLength
    ListPenalty _penalty ->
      Q.zeroLength
  HListHBaseElem hBaseElem -> case hBaseElem of
    BoxElem.CharBoxHBaseElem charBox ->
      charBox.boxedDims.boxHeight
  DiscretionaryItemElem discrItem ->
    BoxElem.hBoxNaturalHeight discrItem.noBreakText

data DiscretionaryItem = DiscretionaryItem
  { preBreakText,
    postBreakText,
    noBreakText ::
      Seq BoxElem.HBoxElem
  }
  deriving stock (Show, Generic)

data DiscretionaryTextPart
  = DiscretionaryTextPartCharacter (Box.Boxed BoxElem.CharBoxContents)
  | DiscretionaryTextBaseELem
  deriving stock (Show, Generic)

discretionaryHyphenItem :: Box.Boxed BoxElem.CharBoxContents -> DiscretionaryItem
discretionaryHyphenItem c =
  DiscretionaryItem
    (BoxElem.singletonHBoxElemSeq $ BoxElem.HBoxHBaseElem $ BoxElem.CharBoxHBaseElem c)
    mempty
    mempty

discretionaryItemPenalty :: FiniteBadnessVal -> FiniteBadnessVal -> DiscretionaryItem -> FiniteBadnessVal
discretionaryItemPenalty hyphenPenalty explicitHyphenPenalty item = case item.preBreakText of
  Empty -> explicitHyphenPenalty
  _ -> hyphenPenalty

hBoxElemAsHListElem :: BoxElem.HBoxElem -> HListElem
hBoxElemAsHListElem = \case
  BoxElem.HVBoxElem (BoxElem.VBoxBaseElem baseElem) ->
    HVListElem $ VListBaseElem baseElem
  BoxElem.HVBoxElem (BoxElem.VBoxSetGlueElem setGlue) ->
    HVListElem $ ListGlue setGlue.glue
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

hListNaturalDepth :: Seq HListElem -> Q.Length
hListNaturalDepth hList =
  -- The empty (Seq HListElem) has zero depth.
  fromMaybe Q.zeroLength $
    maximumOf (traversed % to hListElemNaturalDepth) hList

hListNaturalHeight :: Seq HListElem -> Q.Length
hListNaturalHeight hList =
  -- The empty (Seq HListElem) has zero height.
  fromMaybe Q.zeroLength $
    maximumOf (traversed % to hListElemNaturalHeight) hList

hListNaturalWidth :: Seq HListElem -> Q.Length
hListNaturalWidth =
  foldMapOf traversed hListElemNaturalWidth

fmtHListOneLine :: Fmt (Seq HListElem)
fmtHListOneLine = F.prefixed "\\hlist" $ F.braced fmtHListElemsOneLine

fmtHListMultiLine :: Fmt (Seq HListElem)
fmtHListMultiLine = F.prefixed "\\hlist\n=======\n" (F.unlined fmtHListElem)

fmtHListElemsOneLine :: Fmt (Seq HListElem)
fmtHListElemsOneLine = F.commaSpaceSep fmtHListElem

fmtHListElem :: Fmt HListElem
fmtHListElem = F.later $ \case
  HVListElem vEl -> bformat fmtVListElem vEl
  HListHBaseElem e -> bformat BoxElem.fmtHBaseElem e
  DiscretionaryItemElem discrItem ->
    bformat fmtDiscretionaryItem discrItem

vListNaturalWidth :: Seq VListElem -> Q.Length
vListNaturalWidth vList =
  -- The empty (Seq VListElem) has zero width.
  fromMaybe Q.zeroLength $
    maximumOf (traversed % to vListElemNaturalWidth) vList

vListNaturalHeight :: Seq VListElem -> Q.Length
vListNaturalHeight = foldMapOf traversed vListElemNaturalHeight

vListNaturalDepth :: Seq VListElem -> Q.Length
vListNaturalDepth vList =
  fromMaybe Q.zeroLength $
    lastOf (traversed % to vListElemNaturalDepth) vList

vListNetBiFlex :: Seq VListElem -> Q.BiNetFlex
vListNetBiFlex = foldOf (traversed % vListElemBiFlex)
  where
    vListElemBiFlex :: AffineFold VListElem Q.BiNetFlex
    vListElemBiFlex = _Typed @Q.Glue % to Q.asBiNetFlex

vListContainsBoxes :: Seq VListElem -> Bool
vListContainsBoxes = anyOf traversed vListElemIsBox

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
