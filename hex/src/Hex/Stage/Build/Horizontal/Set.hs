module Hex.Stage.Build.Horizontal.Set where

import Hex.Common.Quantity qualified as Q
import Hex.Stage.Build.AnyDirection.Evaluate qualified as Eval
import Hex.Stage.Build.BoxElem qualified as BoxElem
import Hex.Stage.Build.Horizontal.Evaluate
import Hex.Stage.Build.ListElem qualified as ListElem
import Hexlude

setList :: ListElem.HList -> Q.Length -> (BoxElem.HBoxElemSeq, Eval.GlueFlexSpec)
setList hList desiredWidth =
  let flexSpec = listFlexSpec hList desiredWidth
   in (setListElems flexSpec hList, flexSpec)

setListElems ::  Eval.GlueFlexSpec -> ListElem.HList -> BoxElem.HBoxElemSeq
setListElems flexSpec hList = BoxElem.HBoxElemSeq $ seqOf (ListElem.hListElemTraversal % afolding (setElem flexSpec)) hList

setElem :: Eval.GlueFlexSpec -> ListElem.HListElem -> Maybe BoxElem.HBoxElem
setElem flexSpec = \case
  ListElem.HVListElem (ListElem.ListGlue glue) ->
    Just $ BoxElem.HVBoxElem $ BoxElem.BoxGlue (Eval.applyGlueFlexSpec flexSpec glue)
  ListElem.HVListElem (ListElem.ListPenalty _) ->
    Nothing
  ListElem.HVListElem (ListElem.VListBaseElem e) ->
    Just $ BoxElem.HVBoxElem $ BoxElem.VBoxBaseElem e
  ListElem.HListHBaseElem hBaseElem ->
    Just $ BoxElem.HBoxHBaseElem hBaseElem
