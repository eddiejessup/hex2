module Hex.Stage.Build.Horizontal.Paragraph.Types where

import Hex.Stage.Build.AnyDirection.Breaking.Types qualified as Breaking
import Hex.Stage.Build.BoxElem qualified as H.Inter.B.Box
import Hex.Stage.Build.ListElem qualified as H.Inter.B.List
import Hexlude

-- TODO: Add math formula conditions.
-- TODO: Discretionary break and Math-off.
hListElemToBreakItem :: (Maybe H.Inter.B.List.HListElem, H.Inter.B.List.HListElem, Maybe H.Inter.B.List.HListElem) -> Maybe Breaking.BreakItem
hListElemToBreakItem = \case
  (Just x, H.Inter.B.List.HVListElem (H.Inter.B.List.ListGlue g), _)
    | not (hListElemisDiscardable x) ->
        Just $ Breaking.GlueBreak g
  (_, H.Inter.B.List.HVListElem (H.Inter.B.List.VListBaseElem (H.Inter.B.Box.ElemKern k)), Just (H.Inter.B.List.HVListElem (H.Inter.B.List.ListGlue _))) ->
    Just $ Breaking.KernBreak k
  (_, H.Inter.B.List.HVListElem (H.Inter.B.List.ListPenalty p), _) ->
    Just $ Breaking.PenaltyBreak p
  _ -> Nothing

hListElemisDiscardable :: H.Inter.B.List.HListElem -> Bool
hListElemisDiscardable (H.Inter.B.List.HVListElem e) = Breaking.vListElemIsDiscardable e
hListElemisDiscardable (H.Inter.B.List.HListHBaseElem (H.Inter.B.Box.ElemCharacter _)) = False
