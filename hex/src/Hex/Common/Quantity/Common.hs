module Hex.Common.Quantity.Common where

import Hexlude

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

data MoveMode
  = Put
  | Set
  deriving stock (Show)

data BoxDim
  = BoxWidth
  | BoxHeight
  | BoxDepth
  deriving stock (Show, Eq, Generic)
