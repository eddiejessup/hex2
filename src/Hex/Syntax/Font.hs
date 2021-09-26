{-# LANGUAGE UndecidableInstances #-}
module Hex.Syntax.Font where

import Hexlude
import Hex.Syntax.Common
import Hex.Syntax.Quantity

data FontFileSpec p = FontFileSpec (FontSpecification p) HexFilePath
  deriving stock (Generic)

data FontSpecification p = NaturalFont | FontAt (HexPassLength p) | FontScaled (HexPassInt p)
  deriving stock (Generic)
