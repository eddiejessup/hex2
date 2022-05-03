module Hex.Stage.Expand.Interface where

import Hex.Common.HexState.Interface.Resolve.PrimitiveToken qualified as PT
import Hex.Stage.Lex.Interface.Extract qualified as Lex
import Hexlude

class MonadPrimTokenSource m where
  getTokenInhibited :: m (Maybe Lex.LexToken)

  getPrimitiveToken :: m (Maybe (Lex.LexToken, PT.PrimitiveToken))
