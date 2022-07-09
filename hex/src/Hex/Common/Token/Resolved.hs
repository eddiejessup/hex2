module Hex.Common.Token.Resolved where

import Hexlude
import qualified Formatting as F
import Hex.Common.Token.Resolved.Expandable
import Hex.Common.Token.Resolved.Primitive

-- The result of resolving a symbol.
data ResolvedToken
  = ExpansionCommandHeadToken ExpansionCommandHeadToken
  | PrimitiveToken PrimitiveToken
  deriving stock (Show, Eq, Generic)

fmtResolvedToken :: Fmt ResolvedToken
fmtResolvedToken = F.later $ \case
  ExpansionCommandHeadToken st -> F.bformat fmtExpansionCommandHeadTokenType st
  PrimitiveToken pt -> F.bformat fmtPrimitiveToken pt
