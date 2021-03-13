module Hex.TFM.Get.Recipe where

import Data.Serialize.Get qualified as Ser
import Hex.TFM.Types
import Hexlude

getExtensibleRecipe :: Ser.Get Recipe
getExtensibleRecipe =
  Recipe <$> Ser.getWord8 <*> Ser.getWord8 <*> Ser.getWord8 <*> Ser.getWord8
