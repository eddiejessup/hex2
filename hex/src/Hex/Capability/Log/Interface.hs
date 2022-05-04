module Hex.Capability.Log.Interface where

import Hexlude

class Monad m => MonadHexLog m where
  logText :: Text -> m ()
