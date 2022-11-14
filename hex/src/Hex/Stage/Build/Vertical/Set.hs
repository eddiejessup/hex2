module Hex.Stage.Build.Vertical.Set where

import Hex.Common.Quantity qualified as Q
import Hex.Stage.Build.AnyDirection.Evaluate qualified as Eval
import Hex.Stage.Build.BoxElem qualified as BoxElem
import Hex.Stage.Build.ListElem qualified as List
import Hex.Stage.Build.Vertical.Evaluate qualified as V
import Hexlude

setList :: Seq List.VListElem -> Q.Length -> (Seq BoxElem.VBoxElem, Eval.GlueFlexSpec)
setList vList desiredHeight =
  let flexSpec = V.listFlexSpec vList desiredHeight
   in (setListElems flexSpec vList, flexSpec)

setListElems :: Eval.GlueFlexSpec -> Seq List.VListElem -> Seq BoxElem.VBoxElem
setListElems flexSpec vList =
  seqOf (traversed % afolding (setElem flexSpec)) vList

setElem :: Eval.GlueFlexSpec -> List.VListElem -> Maybe BoxElem.VBoxElem
setElem flexSpec = \case
  List.ListGlue glue ->
    Just $ BoxElem.VBoxSetGlueElem $ Eval.applyGlueFlexSpec flexSpec glue
  List.ListPenalty _ ->
    Nothing
  List.VListBaseElem e ->
    Just $ BoxElem.VBoxBaseElem e
