module Hex.Capability.Log.Interface where

import Hexlude

class Monad m => MonadHexLog m where
  log :: Text -> m ()

  logInternalState :: m ()

instance MonadHexLog m => MonadHexLog (StateT s m) where
  log x = lift $ log x

  logInternalState = lift logInternalState
