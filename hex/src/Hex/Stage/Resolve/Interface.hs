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

getResolvedToken :: MonadResolvedTokenSource m => ResolutionMode -> m (Maybe (Lex.LexToken, ResolvedToken))
getResolvedToken resMode = do
  getLexToken >>= \case
    Nothing -> pure Nothing
    Just lt ->
      resolveLexToken resMode lt >>= \case
        Left _ -> pure Nothing
        Right rt -> pure $ Just (lt, rt)
