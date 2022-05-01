module Hex.Stage.Expand.Interface where

import Hex.Common.HexState.Interface.Resolve.PrimitiveToken qualified as PT
import Hexlude
import qualified Hex.Stage.Lex.Interface.Extract as Lex
import qualified Hex.Common.HexState.Interface.Resolve as Res

class MonadPrimTokenSource m where
  getPrimitiveToken :: m (Maybe (Lex.LexToken, Res.ResolvedToken, PT.PrimitiveToken))
