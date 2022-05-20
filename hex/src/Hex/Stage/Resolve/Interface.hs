module Hex.Stage.Resolve.Interface where

import Hexlude
import qualified Hex.Stage.Lex.Interface.Extract as Lex
import Hex.Common.HexState.Interface.Resolve (ResolvedToken)
import Hex.Stage.Lex.Interface (MonadLexTokenSource (..))
import qualified Formatting as F
import Hex.Stage.Lex.Interface.Extract (LexToken)

data ResolutionError = ResolutionError LexToken
  deriving stock (Show, Generic)

fmtResolutionError :: Fmt ResolutionError
fmtResolutionError = F.shown

class (Monad m) => MonadResolve m where
  resolveLexToken :: Lex.LexToken -> m (Either ResolutionError ResolvedToken)

-- If we can resolve lex-tokens, and we have a source of lex-tokens, we can
-- provide a stream of resolved-tokens.
getMayResolvedToken :: (MonadResolve m, MonadLexTokenSource m) => m (Maybe (Lex.LexToken, Either ResolutionError ResolvedToken))
getMayResolvedToken =
  -- Get a lex token.
  getLexToken >>= \case
    -- If no lex token, return nothing.
    Nothing -> pure Nothing
    -- If there is a lex token, try to resolve it.
    Just lt -> do
      errOrResolvedTok <- resolveLexToken lt
      pure $ Just (lt, errOrResolvedTok)
