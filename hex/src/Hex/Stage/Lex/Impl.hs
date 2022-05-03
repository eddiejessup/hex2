{-# LANGUAGE UndecidableInstances #-}
{-# OPTIONS_GHC -Wno-orphans #-}

module Hex.Stage.Lex.Impl where

import Hex.Common.HexState.Interface qualified as HSt
import Hex.Stage.Lex.Impl.CharSource qualified as Impl
import Hex.Stage.Lex.Interface
import Hex.Stage.Lex.Interface.CharSource
import Hex.Stage.Lex.Interface.Extract qualified as Lex
import Hexlude

newtype MonadLexTokenSourceT m a = MonadLexTokenSourceT {unMonadLexTokenSourceT :: m a}
  deriving newtype (Functor, Applicative, Monad, MonadIO, MonadState st, MonadError e, HSt.MonadHexState)

instance
  ( Monad (MonadLexTokenSourceT m),
    MonadState st (MonadLexTokenSourceT m),
    HasType CharSource st,
    MonadError e (MonadLexTokenSourceT m),
    AsType Lex.LexError e,
    HSt.MonadHexState (MonadLexTokenSourceT m)
  ) =>
  MonadLexTokenSource (MonadLexTokenSourceT m)
  where
  getLexToken = do
    Impl.extractLexToken >>= \case
      Nothing -> pure Nothing
      Just lt -> do
        HSt.setLastFetchedLexTok lt
        pure $ Just lt

  insertLexTokensToSource = Impl.insertLexTokensToSource

  insertLexTokenToSource = Impl.insertLexTokenToSource

  getSource = use (typed @CharSource)

  putSource = assign' (typed @CharSource)
