{-# LANGUAGE UndecidableInstances #-}

module Hex.Capability.Log.Impl where

import Hex.Capability.Log.Interface (MonadHexLog (..))
import Hexlude

newtype MonadHexLogT m a = MonadHexLogT {unMonadHexLogT :: m a}
  deriving newtype (
    Functor,
    Applicative,
    Monad,
    MonadIO,
    MonadReader st,
    MonadState st,
    MonadError e
  )

instance
  ( Monad (MonadHexLogT m),
    MonadReader r (MonadHexLogT m),
    HasType Handle r,
    MonadIO m
  ) =>
  MonadHexLog (MonadHexLogT m)
  where
  logText msg = do
    logFileHandle <- know $ typed @Handle
    liftIO $ hPutStrLn logFileHandle msg
