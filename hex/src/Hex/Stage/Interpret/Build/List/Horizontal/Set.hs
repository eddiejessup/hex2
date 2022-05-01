module Hex.Stage.Interpret.Build.List.Horizontal.Set where

import Hex.Stage.Interpret.Build.Box.Elem qualified as H.Inter.B.Box
import Hex.Stage.Interpret.Build.List.Elem qualified as H.Inter.B.List
import Hex.Stage.Interpret.Build.List.Horizontal.Badness
import Hex.Stage.Interpret.Build.List.Horizontal.Evaluate
import Hex.Common.Quantity qualified as H.Q
import Hexlude

setList :: H.Inter.B.List.HList -> H.Q.Length -> (H.Inter.B.Box.HBoxElemSeq, Badness)
setList hList desiredWidth =
  let (flexSpec, badness) = listFlexSpec hList desiredWidth

      outElems = seqOf (H.Inter.B.List.hListElemTraversal % afolding (setElem flexSpec)) hList
   in (H.Inter.B.Box.HBoxElemSeq outElems, badness)

setElem :: GlueFlexSpec -> H.Inter.B.List.HListElem -> Maybe H.Inter.B.Box.HBoxElem
setElem flexSpec = \case
  H.Inter.B.List.HVListElem (H.Inter.B.List.ListGlue glue) ->
    Just $ H.Inter.B.Box.HVBoxElem $ H.Inter.B.Box.BoxGlue (applyGlueFlexSpec flexSpec glue)
  H.Inter.B.List.HVListElem (H.Inter.B.List.ListPenalty _) ->
    Nothing
  H.Inter.B.List.HVListElem (H.Inter.B.List.VListBaseElem e) ->
    Just $ H.Inter.B.Box.HVBoxElem $ H.Inter.B.Box.VBoxBaseElem e
  H.Inter.B.List.HListHBaseElem hBaseElem ->
    Just $ H.Inter.B.Box.HBoxHBaseElem hBaseElem
