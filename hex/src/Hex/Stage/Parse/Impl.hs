{-# LANGUAGE UndecidableInstances #-}
{-# OPTIONS_GHC -Wno-orphans #-}

module Hex.Stage.Parse.Impl where

import Hex.Stage.Expand.Impl.Parsing (MonadPrimTokenParse (..))
import Hex.Stage.Parse.Impl.Parsers.Command qualified as Parsers.Command
import Hex.Stage.Parse.Interface

instance (MonadPrimTokenParse m) => MonadParse m where
  parseCommand = Parsers.Command.parseCommand

-- getSource = Expand.getSource
-- putSource = Expand.putSource
-- insertLexTokenToSource = Expand.insertLexTokenToSource
-- insertLexTokensToSource = Expand.insertLexTokensToSource
