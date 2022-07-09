module Hex.Common.HexState.Interface.Resolve where

import Formatting qualified as F
import Hex.Common.Codes qualified as Code
import Hex.Common.Token.Lexed qualified as LT
import Hex.Common.Token.Resolved qualified as RT
import Hexlude

-- Symbol to be resolved.
data ControlSymbol
  = ActiveCharacterSymbol Code.CharCode
  | ControlSequenceSymbol LT.ControlSequence
  deriving stock (Show, Eq, Ord, Generic)

fmtControlSymbol :: Fmt ControlSymbol
fmtControlSymbol = F.later $ \case
  ActiveCharacterSymbol c -> "Active '" <> F.bformat Code.fmtCharCode c <> "'"
  ControlSequenceSymbol cs -> F.bformat LT.fmtControlSequence cs

type SymbolMap = Map ControlSymbol RT.ResolvedToken
