module Hex.Categorise.Types where

import Protolude
import qualified Hex.Codes as Code

data RawCharCat
  = RawCharCat
      { rawCCChar :: Code.CharCode
      , rawCCCat :: Code.CatCode
      }
  deriving stock (Show, Generic)

data EndOfInput = EndOfInput
  deriving stock (Show, Eq, Generic)

newtype CatFailure
  = CatEndOfInputFailure EndOfInput
  deriving stock (Show, Eq, Generic)
