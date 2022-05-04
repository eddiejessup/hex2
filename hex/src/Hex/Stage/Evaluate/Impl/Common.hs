module Hex.Stage.Evaluate.Impl.Common where

import Hexlude

data EvaluationError
  = ValueNotInRange
  deriving stock (Show, Generic)
