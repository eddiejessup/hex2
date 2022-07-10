module Hex.Common.HexInput.Impl.Categorise where

import Formatting qualified as F
import Hex.Common.Codes qualified as Code
import Hexlude
import qualified Hex.Common.HexInput.Interface as HIn

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

peekForCharCatOnCurrentLine :: HIn.MonadHexInput m => m (Maybe RawCharCat)
peekForCharCatOnCurrentLine = notImplemented "peekForCharCatOnCurrentLine"

extractCharCatFromCurrentLine :: HIn.MonadHexInput m => m (Maybe RawCharCat)
extractCharCatFromCurrentLine = notImplemented "extractCharCatFromCurrentLine"
