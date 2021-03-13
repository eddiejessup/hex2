module Hex.Categorise.Types where

import Hex.Codes qualified as Code
import Hexlude

data RawCharCat = RawCharCat
  { rawCCChar :: Code.CharCode,
    rawCCCat :: Code.CatCode
  }
  deriving stock (Show, Generic)

data EndOfInput = EndOfInput
  deriving stock (Show, Eq, Generic)

newtype CatFailure
  = CatEndOfInputFailure EndOfInput
  deriving stock (Show, Eq, Generic)
