{-# LANGUAGE UndecidableInstances #-}

module Hex.Stage.Lex.Impl where

import Hex.Capability.Log.Interface (MonadHexLog)
import Hex.Capability.Log.Interface qualified as Log
import Hex.Common.HexState.Interface qualified as HSt
import Hex.Stage.Categorise.Interface qualified as Cat
import Hex.Stage.Lex.Impl.CharSource qualified as Impl
import Hex.Stage.Lex.Interface
import Hex.Stage.Lex.Interface.CharSource
import Hex.Stage.Lex.Interface.Extract qualified as Lex
import Hexlude

newtype MonadLexTokenSourceT m a = MonadLexTokenSourceT {unMonadLexTokenSourceT :: m a}
  deriving newtype
    ( Functor,
      Applicative,
      Monad,
      MonadIO,
      MonadState st,
      MonadError e,
      Cat.MonadCharCatSource,
      HSt.MonadHexState,
      MonadHexLog
    )

instance
  ( Monad (MonadLexTokenSourceT m),
    MonadState st (MonadLexTokenSourceT m),
    HasType CharSource st,
    MonadError e (MonadLexTokenSourceT m),
    AsType Lex.LexError e,
    Cat.MonadCharCatSource (MonadLexTokenSourceT m),
    HSt.MonadHexState (MonadLexTokenSourceT m),
    Log.MonadHexLog (MonadLexTokenSourceT m)
  ) =>
  MonadLexTokenSource (MonadLexTokenSourceT m)
  where
  getLexToken = Impl.extractLexToken

  insertLexTokensToSource = Impl.insertLexTokensToSource

  insertLexTokenToSource = Impl.insertLexTokenToSource

  getSource = use (typed @CharSource)

  putSource = assign' (typed @CharSource)
