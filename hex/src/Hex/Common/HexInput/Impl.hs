{-# LANGUAGE UndecidableInstances #-}

module Hex.Common.HexInput.Impl where

import Hex.Capability.Log.Interface (MonadHexLog)
import Hex.Common.HexState.Interface (MonadHexState)
import Hex.Common.HexInput.Interface (MonadHexInput (..))
import Hexlude
import qualified Hex.Common.HexInput.Interface as HIn

newtype MonadHexInputT m a = MonadHexInputT {unMonadHexInputT :: m a}
  deriving newtype
    ( Functor,
      Applicative,
      Monad,
      MonadIO,
      MonadState st,
      MonadError e,
      MonadHexState,
      MonadHexLog
    )

instance
  ( Monad m,
    MonadError e (MonadHexInputT m),
    AsType HIn.LexError e,
    MonadHexState (MonadHexInputT m)
  ) =>
  MonadHexInput (MonadHexInputT m)
  where
