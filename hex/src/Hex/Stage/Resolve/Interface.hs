module Hex.Stage.Resolve.Interface where

import Formatting qualified as F
import Hex.Common.Token.Lexed qualified as LT
import Hex.Common.Token.Resolved qualified as RT
import Hex.Stage.Lex.Interface (MonadLexTokenSource (..))
import Hexlude

data ResolutionError = ResolutionError LT.LexToken
  deriving stock (Show, Generic)

fmtResolutionError :: Fmt ResolutionError
fmtResolutionError = F.shown

class (Monad m) => MonadResolve m where
  resolveLexToken :: LT.LexToken -> m (Either ResolutionError RT.ResolvedToken)

-- If we can resolve lex-tokens, and we have a source of lex-tokens, we can
-- provide a stream of resolved-tokens.
getMayResolvedToken :: (MonadResolve m, MonadLexTokenSource m) => m (Maybe (LT.LexToken, Either ResolutionError RT.ResolvedToken))
getMayResolvedToken =
  -- Get a lex token.
  getLexToken >>= \case
    -- If no lex token, return nothing.
    Nothing -> pure Nothing
    -- If there is a lex token, try to resolve it.
    Just lt -> do
      errOrResolvedTok <- resolveLexToken lt
      pure $ Just (lt, errOrResolvedTok)
