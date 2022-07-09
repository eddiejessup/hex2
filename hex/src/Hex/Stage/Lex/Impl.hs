{-# LANGUAGE UndecidableInstances #-}

module Hex.Stage.Lex.Impl where

import Hex.Capability.Log.Interface (MonadHexLog)
import Hex.Capability.Log.Interface qualified as Log
import Hex.Common.HexState.Interface qualified as HSt
import Hex.Stage.Categorise.Interface qualified as Cat
import Hex.Stage.Lex.Impl.LexBuffer qualified as Impl
import Hex.Stage.Lex.Interface
import Hex.Stage.Lex.Interface qualified as Lex
import Hexlude
import qualified Hex.Common.HexInput.Interface as HIn

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
  ( Monad m,
    MonadState st (MonadLexTokenSourceT m),
    HIn.MonadHexInput (MonadLexTokenSourceT m),
    MonadError e (MonadLexTokenSourceT m),
    AsType Lex.LexError e,
    Cat.MonadCharCatSource (MonadLexTokenSourceT m),
    HSt.MonadHexState (MonadLexTokenSourceT m),
    Log.MonadHexLog (MonadLexTokenSourceT m)
  ) =>
  MonadLexTokenSource (MonadLexTokenSourceT m)
  where
  getLexToken = Impl.extractLexToken
