module Hex.Stage.Build.Horizontal.Paragraph.Types where

import Hex.Stage.Build.AnyDirection.Breaking.Types qualified as Breaking
import Hex.Stage.Build.BoxElem qualified as BoxElem
import Hex.Stage.Build.ListElem qualified as ListElem
import Hexlude

-- TODO: Add math formula conditions.
-- TODO: Discretionary break and Math-off.
hListElemToBreakItem :: (Maybe ListElem.HListElem, ListElem.HListElem, Maybe ListElem.HListElem) -> Maybe Breaking.BreakItem
hListElemToBreakItem = \case
  (Just x, ListElem.HVListElem (ListElem.ListGlue g), _)
    | not (hListElemisDiscardable x) ->
        Just $ Breaking.GlueBreak g
  (_, ListElem.HVListElem (ListElem.VListBaseElem (BoxElem.ElemKern k)), Just (ListElem.HVListElem (ListElem.ListGlue _))) ->
    Just $ Breaking.KernBreak k
  (_, ListElem.HVListElem (ListElem.ListPenalty p), _) ->
    Just $ Breaking.PenaltyBreak p
  _ -> Nothing

hListElemisDiscardable :: ListElem.HListElem -> Bool
hListElemisDiscardable (ListElem.HVListElem e) = Breaking.vListElemIsDiscardable e
hListElemisDiscardable (ListElem.HListHBaseElem (BoxElem.ElemCharacter _)) = False
