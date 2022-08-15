module Hex.Stage.Build.Horizontal.Paragraph.Types where

import Data.Sequence qualified as Seq
import Formatting qualified as F
import Hex.Common.Quantity qualified as Q
import Hex.Stage.Build.AnyDirection.Breaking.Badness qualified as Bad
import Hex.Stage.Build.BoxElem qualified as BoxElem
import Hex.Stage.Build.ListElem qualified as ListElem
import Hex.Stage.Build.Vertical.Page.Break (VBreakItem (..))
import Hex.Stage.Build.Vertical.Page.Break qualified as V
import Hexlude

data HBreakItem
  = HVBreakItem V.VBreakItem
  | DiscretionaryBreak ListElem.DiscretionaryItem
  deriving stock (Show, Generic)

fmtHBreakItem :: Fmt HBreakItem
fmtHBreakItem = F.later $ \case
  HVBreakItem (GlueBreak glue) -> "Glue-break{" <> F.bformat Q.fmtGlue glue <> "}"
  HVBreakItem (KernBreak kern) -> "Kern-break{" <> F.bformat BoxElem.fmtKern kern <> "}"
  HVBreakItem (PenaltyBreak penalty) -> "Penalty-break{" <> F.bformat ListElem.fmtPenalty penalty <> "}"
  DiscretionaryBreak discrItem -> "Discretionary-break{" <> F.bformat ListElem.fmtDiscretionaryItem discrItem <> "}"

-- TODO: Add math formula conditions.
-- TODO: Math-off.
hListElemToBreakItem :: (Maybe ListElem.HListElem, ListElem.HListElem, Maybe ListElem.HListElem) -> Maybe HBreakItem
hListElemToBreakItem = \case
  (Just x, ListElem.HVListElem (ListElem.ListGlue g), _)
    | not (hListElemIsDiscardable x) ->
        Just $ HVBreakItem $ V.GlueBreak g
  (_, ListElem.HVListElem (ListElem.VListBaseElem (BoxElem.ElemKern k)), Just (ListElem.HVListElem (ListElem.ListGlue _))) ->
    Just $ HVBreakItem $ V.KernBreak k
  (_, ListElem.HVListElem (ListElem.ListPenalty p), _) ->
    Just $ HVBreakItem $ V.PenaltyBreak p
  (_, ListElem.DiscretionaryItemElem item, _) ->
    Just $ DiscretionaryBreak item
  _ ->
    Nothing

hListElemIsDiscardable :: ListElem.HListElem -> Bool
hListElemIsDiscardable = \case
  ListElem.HVListElem e -> V.vListElemIsDiscardable e
  ListElem.HListHBaseElem (BoxElem.ElemCharacter _) -> False
  ListElem.DiscretionaryItemElem _ -> False

hBreakPenalty :: ListElem.Penalty -> ListElem.Penalty -> HBreakItem -> ListElem.Penalty
hBreakPenalty _ _ (HVBreakItem b) = V.vBreakPenalty b
hBreakPenalty hyphenPenalty explicitHyphenPenalty (DiscretionaryBreak discrItem) =
  (ListElem.discretionaryItemPenalty hyphenPenalty explicitHyphenPenalty discrItem)

-- List elements in a break item, assuming we don't treat it as a break item,
-- but just as part of a list.
hBreakItemAsListElemsNoBreak :: HBreakItem -> Seq ListElem.HListElem
hBreakItemAsListElemsNoBreak = \case
  HVBreakItem b ->
    ListElem.HVListElem <$> vBreakItemAsListElemNoBreak b
  DiscretionaryBreak discrItem ->
    ListElem.hBoxElemSeqAsHListElems discrItem.noBreakText
  where
    vBreakItemAsListElemNoBreak :: VBreakItem -> Seq ListElem.VListElem
    vBreakItemAsListElemNoBreak = \case
      GlueBreak glue -> Seq.singleton $ ListElem.ListGlue glue
      KernBreak kern -> Seq.singleton $ ListElem.VListBaseElem $ BoxElem.ElemKern kern
      PenaltyBreak p -> Seq.singleton $ ListElem.ListPenalty p

-- List elements in a break item that will form part of the pre-break line.
-- - Glue doesn't get added before,
-- - Discretionary item adds its pre-break elements.
-- - Otherwise, we add the single break item as an element.
hBreakItemAsListElemsPreBreak :: HBreakItem -> Seq ListElem.HListElem
hBreakItemAsListElemsPreBreak = \case
  HVBreakItem b ->
    ListElem.HVListElem <$> vBreakItemAsListElemPreBreak b
  DiscretionaryBreak discrItem ->
    ListElem.hBoxElemSeqAsHListElems discrItem.preBreakText
  where
    vBreakItemAsListElemPreBreak :: VBreakItem -> Seq ListElem.VListElem
    vBreakItemAsListElemPreBreak = \case
      GlueBreak _ -> mempty
      KernBreak kern -> Seq.singleton $ ListElem.VListBaseElem $ BoxElem.ElemKern kern
      PenaltyBreak p -> Seq.singleton $ ListElem.ListPenalty p

-- List elements in a break item that will form part of the post-break line.
-- - Discretionary item adds its post-break elements.
-- - Otherwise, nothing is added.
hBreakItemAsListElemsPostBreak :: HBreakItem -> Seq ListElem.HListElem
hBreakItemAsListElemsPostBreak = \case
  HVBreakItem _ ->
    mempty
  DiscretionaryBreak discrItem ->
    ListElem.hBoxElemSeqAsHListElems discrItem.postBreakText

hBreakIsAcceptable ::
  ListElem.Penalty ->
  ListElem.Penalty ->
  Q.HexInt ->
  HBreakItem ->
  Bad.Badness ->
  Bool
hBreakIsAcceptable hyphenPenalty explicitHyphenPenalty tolerance breakItem = \case
  Bad.InfiniteBadness ->
    False
  Bad.FiniteBadness b ->
    hBreakPenalty hyphenPenalty explicitHyphenPenalty breakItem < ListElem.Penalty Q.tenKInt
      && b.unFiniteBadnessVal <= tolerance
