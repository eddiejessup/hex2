module Hex.Common.Quantity.MathGlue where

import Hex.Common.Quantity.Glue
import Hex.Common.Quantity.MathLength
import Hexlude

data MathGlue = MathGlue {mgDimen :: MathLength, mgStretch :: PureFlex, mgShrink :: PureFlex}
  deriving stock (Show, Generic)
