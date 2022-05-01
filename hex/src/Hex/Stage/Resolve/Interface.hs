module Hex.Stage.Resolve.Interface where

import Hexlude
import qualified Hex.Stage.Lex.Interface.Extract as Lex
import Hex.Stage.Lex.Interface.CharSource (CharSource)
import Hex.Common.HexState.Interface.Resolve (ResolvedToken)

data ResolutionError = ResolutionError
  deriving stock (Show, Generic)

data ResolutionMode = Resolving | NotResolving
  deriving stock (Show, Eq)

class Monad m => MonadResolvedTokenSource m where
  resolveLexToken :: ResolutionMode -> Lex.LexToken -> m (Either ResolutionError ResolvedToken)

  getLexToken :: m (Maybe Lex.LexToken)

  insertLexTokenToSource :: Lex.LexToken -> m ()

  insertLexTokensToSource :: Seq Lex.LexToken -> m ()

  getSource :: m CharSource

  putSource :: CharSource -> m ()

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
