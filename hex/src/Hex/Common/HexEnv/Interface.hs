{-# LANGUAGE UndecidableInstances #-}

module Hex.Common.HexEnv.Interface where

import Hexlude

data FindFilePolicy
  = NoImplicitExtension
  | WithImplicitExtension Text

class Monad m => MonadHexEnv m where
  findAndReadFile :: FindFilePolicy -> FilePath -> m (Maybe ByteString)
