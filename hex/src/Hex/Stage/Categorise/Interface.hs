module Hex.Stage.Categorise.Interface where

import Hexlude
import Hex.Stage.Categorise.Types (RawCharCat)

class Monad m => MonadCharCatSource m where
  getCharCat :: m (Maybe RawCharCat)
