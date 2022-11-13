module Hex.Stage.Build.Horizontal.Set where

import Hex.Common.Quantity qualified as Q
import Hex.Stage.Build.AnyDirection.Evaluate qualified as Eval
import Hex.Stage.Build.BoxElem qualified as BoxElem
import Hex.Stage.Build.Horizontal.Evaluate
import Hex.Stage.Build.ListElem qualified as ListElem
import Hexlude

setList :: ListElem.HList -> Q.Length -> (Seq BoxElem.HBoxElem, Eval.GlueFlexSpec)
setList hList desiredWidth =
  let flexSpec = listFlexSpec hList desiredWidth
   in (setListElems flexSpec hList, flexSpec)

setListElems :: Eval.GlueFlexSpec -> ListElem.HList -> Seq BoxElem.HBoxElem
setListElems flexSpec hList = seqOf (ListElem.hListElemTraversal % afolding (setElem flexSpec)) hList

setElem :: Eval.GlueFlexSpec -> ListElem.HListElem -> Maybe BoxElem.HBoxElem
setElem flexSpec = \case
  ListElem.HVListElem (ListElem.ListGlue glue) ->
    Just $
      BoxElem.HVBoxElem $
        BoxElem.VBoxSetGlueElem $
          Eval.applyGlueFlexSpec flexSpec glue
  ListElem.HVListElem (ListElem.ListPenalty _) ->
    Nothing
  ListElem.HVListElem (ListElem.VListBaseElem e) ->
    Just $ BoxElem.HVBoxElem $ BoxElem.VBoxBaseElem e
  ListElem.HListHBaseElem hBaseElem ->
    Just $ BoxElem.HBoxHBaseElem hBaseElem
  ListElem.DiscretionaryItemElem _ ->
    Nothing
