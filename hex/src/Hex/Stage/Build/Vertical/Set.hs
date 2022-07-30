module Hex.Stage.Build.Vertical.Set where

import Hex.Common.Quantity qualified as Q
import Hex.Stage.Build.AnyDirection.Evaluate qualified as Eval
import Hex.Stage.Build.BoxElem qualified as Box
import Hex.Stage.Build.ListElem qualified as List
import Hex.Stage.Build.Vertical.Evaluate qualified as V
import Hexlude

setList :: List.VList -> Q.Length -> (Box.VBoxElemSeq, Eval.GlueFlexSpec)
setList vList desiredHeight =
  let flexSpec = V.listFlexSpec vList desiredHeight
   in (setListElems flexSpec vList, flexSpec)

setListElems :: Eval.GlueFlexSpec -> List.VList -> Box.VBoxElemSeq
setListElems flexSpec vList =
  Box.VBoxElemSeq $ seqOf (List.vListElemTraversal % afolding (setElem flexSpec)) vList

setElem :: Eval.GlueFlexSpec -> List.VListElem -> Maybe Box.VBoxElem
setElem flexSpec = \case
  List.ListGlue glue ->
    Just $ Box.BoxGlue (Eval.applyGlueFlexSpec flexSpec glue)
  List.ListPenalty _ ->
    Nothing
  List.VListBaseElem e ->
    Just $ Box.VBoxBaseElem e
