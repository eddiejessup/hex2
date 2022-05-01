module Hex.Stage.Resolve.Interface where

import Hexlude
import qualified Hex.Stage.Lex.Interface.Extract as Lex
import Hex.Common.HexState.Interface.Resolve (ResolvedToken)
import Hex.Stage.Lex.Interface (MonadLexTokenSource (..))
import qualified Formatting as F
import Hex.Stage.Lex.Interface.Extract (LexToken)

data ResolutionError = ResolutionError LexToken
  deriving stock (Show, Generic)

fmtResolutionError :: Fmt ResolutionError r
fmtResolutionError = F.shown

data ResolutionMode = Resolving | NotResolving
  deriving stock (Show, Eq)

-- We require 'MonadLexTokenSource' because this class would otherwise
-- just repeat the same methods:
-- We need users of this class to be able to:
-- - Get a lex-token
-- - Get and put a char-source
-- - Insert lex-tokens to the char-source
class (Monad m, MonadLexTokenSource m) => MonadResolvedTokenSource m where
  resolveLexToken :: ResolutionMode -> Lex.LexToken -> m (Either ResolutionError ResolvedToken)

getMayResolvedToken :: MonadResolvedTokenSource m => ResolutionMode -> m (Maybe (Lex.LexToken, Either ResolutionError ResolvedToken))
getMayResolvedToken resMode = do
  -- Get a lex token.
  getLexToken >>= \case
    -- If no lex token, return nothing.
    Nothing -> pure Nothing
    -- If there is a lex token, try to resolve it.
    Just lt -> do
      errOrResolvedTok <- resolveLexToken resMode lt
      pure $ Just (lt, errOrResolvedTok)

-- Like getMayResolvedToken, but just return nothing if resolution fails.
getResolvedToken :: MonadResolvedTokenSource m => ResolutionMode -> m (Maybe (Lex.LexToken, ResolvedToken))
getResolvedToken resMode = do
  getMayResolvedToken resMode <&> \case
    Nothing -> Nothing
    Just (lt, errOrResolvedTok) ->
      case rightToMaybe errOrResolvedTok of
        Nothing -> Nothing
        Just rt -> Just (lt, rt)
