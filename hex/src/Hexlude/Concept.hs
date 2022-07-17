module Hexlude.Concept where

import GHC.Generics (Generic)
import Prelude

data HDirection
  = Leftward
  | Rightward
  deriving stock (Show, Eq, Generic)

data VDirection
  = Upward
  | Downward
  deriving stock (Show, Eq, Generic)

data Direction
  = Forward
  | Backward
  deriving stock (Show, Eq, Generic)

data Axis
  = Horizontal
  | Vertical
  deriving stock (Show, Eq, Generic)

newtype HexFilePath = HexFilePath FilePath
  deriving stock (Show, Eq, Generic)
