module Hex.Syntax.Common where

import Hexlude

newtype HexFilePath = HexFilePath FilePath
  deriving stock (Generic)

data Pass = Parsed | Evaluated
  deriving stock (Generic)
