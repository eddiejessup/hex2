module Hex.Stage.Expand.Interface where

import Hex.Common.HexState.Interface.Resolve.PrimitiveToken qualified as PT
import Hex.Stage.Lex.Interface.Extract qualified as Lex
import Hexlude
import qualified Hex.Stage.Resolve.Interface as Res

class MonadPrimTokenSource m where
  getTokenNotResolving :: m (Maybe Lex.LexToken)

  getTokenResolving :: m (Maybe (Lex.LexToken, PT.PrimitiveToken))

-- Pick the correct token-fetcher based on resolution mode, and wrap the
-- lex-token as a primitive token if we are not resolving.
getPrimitiveToken :: (Monad m, MonadPrimTokenSource m) => Res.ResolutionMode -> m (Maybe (Lex.LexToken, PT.PrimitiveToken))
getPrimitiveToken = \case
  Res.Resolving -> do
    getTokenResolving
  Res.NotResolving -> do
    getTokenNotResolving >>= \case
      Nothing -> pure Nothing
      Just lt -> pure $ Just (lt, PT.UnresolvedTok lt)
