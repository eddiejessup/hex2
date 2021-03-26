module Hex.Interpret.Build.List.Horizontal.Paragraph.Types where

import Hex.Interpret.Build.Box.Elem qualified as H.Inter.B.Box
import Hex.Interpret.Build.List.Elem qualified as H.Inter.B.List
import Hex.Quantity qualified as H.Q
import Hexlude

data BreakItem
  = GlueBreak H.Q.Glue
  | KernBreak H.Inter.B.Box.Kern
  | PenaltyBreak H.Inter.B.List.Penalty
  deriving stock (Show, Generic)

-- TODO: Add math formula conditions.
-- TODO: Discretionary break and Math-off.
hListElemToBreakItem :: (Maybe H.Inter.B.List.HListElem, H.Inter.B.List.HListElem, Maybe H.Inter.B.List.HListElem) -> Maybe BreakItem
hListElemToBreakItem = \case
  (Just x, H.Inter.B.List.HVListElem (H.Inter.B.List.ListGlue g), _)
    | not (hListElemisDiscardable x) ->
      Just $ GlueBreak g
  (_, H.Inter.B.List.HVListElem (H.Inter.B.List.VListBaseElem (H.Inter.B.Box.ElemKern k)), Just (H.Inter.B.List.HVListElem (H.Inter.B.List.ListGlue _))) ->
    Just $ KernBreak k
  (_, H.Inter.B.List.HVListElem (H.Inter.B.List.ListPenalty p), _) ->
    Just $ PenaltyBreak p
  _ -> Nothing

hListElemisDiscardable :: H.Inter.B.List.HListElem -> Bool
hListElemisDiscardable (H.Inter.B.List.HVListElem e) = vListElemIsDiscardable e
hListElemisDiscardable (H.Inter.B.List.HListHBaseElem (H.Inter.B.Box.ElemCharacter _)) = False

vListElemIsDiscardable :: H.Inter.B.List.VListElem -> Bool
vListElemIsDiscardable = \case
  H.Inter.B.List.ListGlue _ -> True
  H.Inter.B.List.ListPenalty _ -> True
  H.Inter.B.List.VListBaseElem (H.Inter.B.Box.ElemKern _) -> True
  _ -> False
