module Hex.Stage.Categorise.Interface where

import Formatting qualified as F
import Hex.Common.Codes qualified as Code
import Hexlude

data RawCharCat = RawCharCat
  { rawCCChar :: Code.CharCode,
    rawCCCat :: Code.CatCode
  }
  deriving stock (Show, Eq, Generic)

fmtRawCharCat :: Fmt RawCharCat
fmtRawCharCat =
  let f1 = F.accessed (.rawCCChar) Code.fmtCharCode
      f2 = F.accessed (.rawCCCat) Code.fmtCatCode
   in F.parenthesised $ f1 <> F.fconst ", " <> f2

class Monad m => MonadCharCatSource m where
  getCharCat :: m (Maybe RawCharCat)

  peekCharCat :: m (Maybe RawCharCat)
