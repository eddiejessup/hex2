{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE UndecidableInstances #-}

module Hex.Common.HexEnv.Interface where

import Hexlude

data FindFilePolicy
  = NoImplicitExtension
  | WithImplicitExtension Text

data EHexEnv :: Effect where
  FindAndReadFile :: FindFilePolicy -> FilePath -> EHexEnv m (Maybe ByteString)
  FindAndOpenFile :: FindFilePolicy -> FilePath -> IOMode -> EHexEnv m (Maybe Handle)

makeEffect ''EHexEnv
