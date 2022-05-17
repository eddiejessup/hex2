module Hex.Capability.Log.Interface where

import Hexlude

class Monad m => MonadHexLog m where
  log :: Text -> m ()

  logInternalState :: m ()
