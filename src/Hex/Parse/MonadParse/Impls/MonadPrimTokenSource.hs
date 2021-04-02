{-# LANGUAGE UndecidableInstances #-}
{-# OPTIONS_GHC -Wno-orphans #-}
module Hex.Parse.MonadParse.Impls.MonadPrimTokenSource where

import Hex.Parse.MonadPrimTokenSource.Interface qualified as H.Par.PrimTokSrc
import Hexlude
import Hex.Parse.MonadParse.Interface
import qualified Hex.Parse.Parsers.Command as H.Par.Par

instance (Monad m, H.Par.PrimTokSrc.MonadPrimTokenSource m) => MonadParse m where
  parseCommand = H.Par.Par.parseCommand
