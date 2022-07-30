module Hex.Stage.Build.Vertical.Evaluate where

import Hex.Common.Quantity qualified as Q
import Hex.Stage.Build.AnyDirection.Evaluate qualified as Eval
import Hex.Stage.Build.ListElem qualified as ListElem
import Hexlude

listFlexSpec :: ListElem.VList -> Q.Length -> Eval.GlueFlexSpec
listFlexSpec vList desiredWidth =
  Eval.glueFlexSpec ((ListElem.vListNaturalHeight vList) ~~ desiredWidth) (ListElem.vListNetBiFlex vList)
