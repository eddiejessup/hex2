module Hex.Common.TFM.Get.Types where

import Hexlude

data Tag
  = Plain
  | LigKern
  | Chain
  | Extensible
  deriving stock (Show)
