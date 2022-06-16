module Hex.Stage.Build.Horizontal.Set where

import Hex.Common.Quantity qualified as Q
import Hex.Stage.Build.BoxElem qualified as H.Inter.B.Box
import Hex.Stage.Build.Horizontal.Badness
import Hex.Stage.Build.Horizontal.Evaluate
import Hex.Stage.Build.ListElem qualified as H.Inter.B.List
import Hexlude

setList :: H.Inter.B.List.HList -> Q.Length -> (H.Inter.B.Box.HBoxElemSeq, Badness)
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
