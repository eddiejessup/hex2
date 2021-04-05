{-# LANGUAGE UndecidableInstances #-}

module Hex.Parse.MonadResolvedTokenSource.Interface where

import Hex.Lex.Types qualified as Lex
import Hex.Parse.CharSource
import Hex.Symbol.Resolve as Sym.Res
import Hex.Symbol.Tokens qualified as Sym.Tok
import Hexlude

data ResolutionError = ResolutionError
  deriving stock (Show, Generic)

class Monad m => MonadResolvedTokenSource m where
  getLexToken :: m (Maybe Lex.LexToken)

  resolveLexToken :: Sym.Res.ResolutionMode -> Lex.LexToken -> m (Either ResolutionError Sym.Tok.ResolvedToken)

  insertLexTokenToSource :: Lex.LexToken -> m ()

  insertLexTokensToSource :: Seq Lex.LexToken -> m ()

  getSource :: m CharSource

  putSource :: CharSource -> m ()

getResolvedToken :: MonadResolvedTokenSource m => ResolutionMode -> m (Maybe (Lex.LexToken, Sym.Tok.ResolvedToken))
getResolvedToken resMode = do
  getLexToken >>= \case
    Nothing -> pure Nothing
    Just lt ->
      resolveLexToken resMode lt >>= \case
        Left _ -> pure Nothing
        Right rt -> pure $ Just (lt, rt)
