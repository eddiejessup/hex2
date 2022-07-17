module Hex.Stage.Build.Vertical.Evaluate where

import Hex.Common.Box qualified as Box
import Hex.Common.Quantity qualified as Q
import Hex.Stage.Build.AnyDirection.Evaluate qualified as Eval
import Hex.Stage.Build.BoxElem qualified as Box
import Hex.Stage.Build.ListElem qualified as List
import Hexlude

vListNaturalHeight :: List.VList -> Q.Length
vListNaturalHeight = foldMapOf List.vListElemTraversal vListElemNaturalHeight

vListElemNaturalHeight :: List.VListElem -> Q.Length
vListElemNaturalHeight = \case
  List.ListGlue glue ->
    glue.gDimen
  List.ListPenalty _ ->
    Q.zeroLength
  List.VListBaseElem (Box.ElemBox b) ->
    b.unBaseBox.boxHeight
  List.VListBaseElem (Box.ElemFontDefinition _) ->
    Q.zeroLength
  List.VListBaseElem (Box.ElemFontSelection _) ->
    Q.zeroLength
  List.VListBaseElem (Box.ElemKern kern) ->
    kern.unKern

vListNetBiFlex :: List.VList -> Q.BiNetFlex
vListNetBiFlex = foldOf (List.vListElemTraversal % vListElemBiFlex)
  where
    vListElemBiFlex :: AffineFold List.VListElem Q.BiNetFlex
    vListElemBiFlex = _Typed @Q.Glue % to Q.asBiNetFlex

listFlexSpec :: List.VList -> Q.Length -> Eval.GlueFlexSpec
listFlexSpec vList desiredWidth =
  Eval.glueFlexSpec ((vListNaturalHeight vList) ~~ desiredWidth) (vListNetBiFlex vList)
