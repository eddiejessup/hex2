module Hex.Quantity.MathGlue where

import Hex.Quantity.MathLength
import Hex.Quantity.Glue
import Hexlude

data MathGlue = MathGlue {mgDimen :: MathLength, mgStretch :: Flex, mgShrink :: Flex}
  deriving stock (Show, Eq, Generic)
