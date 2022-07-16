module Hex.Stage.Build.AnyDirection.Breaking.Types where

import Hex.Common.Quantity qualified as Q
import Hex.Stage.Build.BoxElem qualified as H.Inter.B.Box
import Hex.Stage.Build.ListElem qualified as Elem
import Hexlude

data BreakItem
  = GlueBreak Q.Glue
  | KernBreak H.Inter.B.Box.Kern
  | PenaltyBreak Elem.Penalty
  deriving stock (Show, Generic)

vListElemToBreakItem :: (Maybe Elem.VListElem, Elem.VListElem, Maybe Elem.VListElem) -> Maybe BreakItem
vListElemToBreakItem = \case
  (Just x, Elem.ListGlue g, _)
    | not (vListElemIsDiscardable x) ->
        Just $ GlueBreak g
  (_, Elem.VListBaseElem (H.Inter.B.Box.ElemKern k), Just (Elem.ListGlue _)) ->
    Just $ KernBreak k
  (_, Elem.ListPenalty p, _) ->
    Just $ PenaltyBreak p
  _ ->
    Nothing

vListElemIsDiscardable :: Elem.VListElem -> Bool
vListElemIsDiscardable = \case
  Elem.ListGlue _ -> True
  Elem.ListPenalty _ -> True
  Elem.VListBaseElem (H.Inter.B.Box.ElemKern _) -> True
  _ -> False

breakPenalty :: BreakItem -> Int
breakPenalty (PenaltyBreak (Elem.Penalty (Q.HexInt p))) = p
breakPenalty (GlueBreak _) = 0
breakPenalty (KernBreak _) = 0
