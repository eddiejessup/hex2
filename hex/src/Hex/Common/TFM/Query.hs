module Hex.Common.TFM.Query where

import Hex.Common.Codes qualified as Code
import Hex.Common.TFM.Types
import Hexlude

fontCharMetrics :: Font -> Code.CharCode -> Maybe Character
fontCharMetrics fontMetrics chrCode =
  fontMetrics ^. #characters % at' (Code.codeInt chrCode)
