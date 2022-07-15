{-# LANGUAGE UndecidableInstances #-}

module Hex.Common.HexEnv.Interface where

import Hexlude

data FindFilePolicy
  = NoImplicitExtension
  | WithImplicitExtension Text

class Monad m => MonadHexEnv m where
  findFilePath :: FindFilePolicy -> FilePath -> m (Maybe FilePath)
