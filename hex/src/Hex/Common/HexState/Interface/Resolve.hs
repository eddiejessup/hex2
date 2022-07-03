module Hex.Common.HexState.Interface.Resolve where

import Hexlude
import Hex.Common.HexState.Interface.Resolve.PrimitiveToken
import Hex.Common.HexState.Interface.Resolve.ExpandableToken
import qualified Hex.Common.Codes as Code
import qualified Hex.Stage.Lex.Interface.Extract as Lex
import qualified Formatting as F

-- Symbol to be resolved.
data ControlSymbol
  = ActiveCharacterSymbol Code.CharCode
  | ControlSequenceSymbol Lex.ControlSequence
  deriving stock (Show, Eq, Ord, Generic)

fmtControlSymbol :: Fmt ControlSymbol
fmtControlSymbol = F.later $ \case
  ActiveCharacterSymbol c -> "Active '" <> F.bformat Code.fmtCharCode c <> "'"
  ControlSequenceSymbol cs -> F.bformat Lex.fmtControlSequence cs

-- The result of resolving a symbol.
data ResolvedToken
  = ExpansionCommandHeadToken ExpansionCommandHeadToken
  | PrimitiveToken PrimitiveToken
  deriving stock (Show, Eq, Generic)

fmtResolvedToken :: Fmt ResolvedToken
fmtResolvedToken = F.later $ \case
  ExpansionCommandHeadToken st -> F.bformat F.shown st
  PrimitiveToken pt -> F.bformat fmtPrimitiveToken pt

type SymbolMap = Map ControlSymbol ResolvedToken
