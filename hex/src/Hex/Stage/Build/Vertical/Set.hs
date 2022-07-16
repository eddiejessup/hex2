module Hex.Stage.Build.Vertical.Set where

import Hex.Common.Quantity qualified as Q
import Hex.Stage.Build.AnyDirection.Breaking.Badness qualified as Bad
import Hex.Stage.Build.AnyDirection.Evaluate qualified as Eval
import Hex.Stage.Build.BoxElem qualified as Box
import Hex.Stage.Build.ListElem qualified as List
import Hex.Stage.Build.Vertical.Evaluate qualified as V
import Hexlude

setList :: List.VList -> Q.Length -> (Box.VBoxElemSeq, Bad.Badness)
setList vList desiredHeight =
  let flexSpec = V.listFlexSpec vList desiredHeight
      outElems = seqOf (List.vListElemTraversal % afolding (setElem flexSpec)) vList
   in (Box.VBoxElemSeq outElems, Eval.glueFlexSpecBadness flexSpec)

setElem :: Eval.GlueFlexSpec -> List.VListElem -> Maybe Box.VBoxElem
setElem flexSpec = \case
  List.ListGlue glue ->
    Just $ Box.BoxGlue (Eval.applyGlueFlexSpec flexSpec glue)
  List.ListPenalty _ ->
    Nothing
  List.VListBaseElem e ->
    Just $ Box.VBoxBaseElem e
