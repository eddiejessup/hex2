module Hex.Common.TFM.Get.Recipe where

import Effectful.Serialize.Get qualified as Get
import Hex.Common.TFM.Types
import Hexlude

getExtensibleRecipe :: (Get.Get :> es) => Eff es Recipe
getExtensibleRecipe =
  Recipe <$> Get.getWord8 <*> Get.getWord8 <*> Get.getWord8 <*> Get.getWord8
