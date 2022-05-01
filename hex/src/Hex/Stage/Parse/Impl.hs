{-# LANGUAGE UndecidableInstances #-}
{-# OPTIONS_GHC -Wno-orphans #-}

module Hex.Stage.Parse.Impl where

import Hex.Stage.Parse.Interface
import Hex.Stage.Parse.Impl.Parsers.Command qualified as Parsers.Command
import Hexlude
import Hex.Stage.Expand.Interface qualified as Expand

instance (Expand.MonadPrimTokenSource m) => MonadParse m where
  parseCommand = Parsers.Command.parseCommand
  -- getSource = Expand.getSource
  -- putSource = Expand.putSource
  -- insertLexTokenToSource = Expand.insertLexTokenToSource
  -- insertLexTokensToSource = Expand.insertLexTokensToSource
