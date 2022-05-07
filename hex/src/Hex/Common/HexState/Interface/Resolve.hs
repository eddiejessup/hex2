module Hex.Common.HexState.Interface.Resolve where

import Hexlude
import Hex.Common.HexState.Interface.Resolve.PrimitiveToken
import Hex.Common.HexState.Interface.Resolve.SyntaxToken
import qualified Hex.Common.Codes as Codes
import qualified Hex.Stage.Lex.Interface.Extract as Lex
import qualified Formatting as F

-- Symbol to be resolved.
data ControlSymbol
  = ActiveCharacterSymbol Codes.CharCode
  | ControlSequenceSymbol Lex.ControlSequence
  deriving stock (Show, Eq, Ord, Generic)

-- The result of resolving a symbol.
data ResolvedToken
  = SyntaxCommandHeadToken SyntaxCommandHeadToken
  | PrimitiveToken PrimitiveToken
  deriving stock (Show, Eq, Generic)

fmtResolvedToken :: Fmt ResolvedToken
fmtResolvedToken = F.later $ \case
  SyntaxCommandHeadToken st -> F.bformat F.shown st
  PrimitiveToken pt -> F.bformat fmtPrimitiveToken pt

type SymbolMap = Map ControlSymbol ResolvedToken
