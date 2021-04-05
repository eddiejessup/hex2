{-# LANGUAGE UndecidableInstances #-}
{-# OPTIONS_GHC -Wno-orphans #-}

module Hex.Parse.MonadParse.Impls.MonadPrimTokenSource where

import Hex.Parse.MonadParse.Interface
import Hex.Parse.MonadPrimTokenSource.Interface qualified as H.Par.PrimTokSrc
import Hex.Parse.Parsers.Command qualified as H.Par.Par
import Hexlude

instance (Monad m, H.Par.PrimTokSrc.MonadPrimTokenSource m) => MonadParse m where
  parseCommand = H.Par.Par.parseCommand
