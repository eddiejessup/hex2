module Hex.Stage.Expand.Interface where

import Hex.Common.HexState.Interface.Resolve qualified as Res
import Hex.Common.HexState.Interface.Resolve.PrimitiveToken qualified as PT
import Hex.Stage.Lex.Interface.Extract qualified as Lex
import Hexlude

class MonadPrimTokenSource m where
  getPrimitiveToken :: m (Maybe (Lex.LexToken, Res.ResolvedToken, PT.PrimitiveToken))

  getLastPrimitiveToken :: m (Maybe PT.PrimitiveToken)
