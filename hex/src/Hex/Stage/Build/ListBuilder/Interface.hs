{-# LANGUAGE UndecidableInstances #-}

module Hex.Stage.Build.ListBuilder.Interface where

import Hex.Stage.Build.ListElem qualified as B
import Hexlude

class Monad m => MonadHexListBuilder m where
  addVListElement :: B.VListElem -> m ()

class MonadHexListBuilder m => MonadHListBuilder m where
  addHListElement :: B.HListElem -> m ()
