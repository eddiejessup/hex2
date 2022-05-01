module Hex.Stage.Categorise.Types where

import Formatting qualified as F
import Hex.Common.Codes qualified as Code
import Hexlude

data RawCharCat = RawCharCat
  { rawCCChar :: Code.CharCode,
    rawCCCat :: Code.CatCode
  }
  deriving stock (Show, Eq, Generic)

fmtRawCharCat :: Fmt RawCharCat r
fmtRawCharCat =
  let
    f1 = F.accessed (.rawCCChar) Code.fmtCharCode
    f2 = F.accessed (.rawCCCat) Code.fmtCatCode
  in
    F.parenthesised $ f1 <> F.fconst ", " <> f2

data EndOfInput = EndOfInput
  deriving stock (Show, Eq, Generic)

newtype CatFailure
  = CatEndOfInputFailure EndOfInput
  deriving stock (Show, Eq, Generic)

