{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE UndecidableInstances #-}

module Hex.Common.HexEnv.Interface where

import Hexlude

data FindFilePolicy
  = NoImplicitExtension
  | WithImplicitExtension Text

data EHexEnv :: Effect where
  FindAndReadFile :: FindFilePolicy -> HexFilePath -> EHexEnv m (Maybe ByteString)
  FindAndOpenFile :: FindFilePolicy -> HexFilePath -> IOMode -> EHexEnv m (Maybe Handle)

makeEffect ''EHexEnv
