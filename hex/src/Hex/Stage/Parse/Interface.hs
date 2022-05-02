module Hex.Stage.Parse.Interface where

import Hex.Stage.Parse.Interface.AST.Command (Command)
import Hexlude
import Hex.Stage.Lex.Interface (MonadLexTokenSource)

-- Require 'MonadLexTokenSource' just to avoid the class repeating the same methods,
-- which are part of its interface.
-- Things clients should be able to do:
-- - Get a lex-token
-- - Get and put a char-source
-- - Insert lex-tokens to the char-source
class (Monad m, MonadLexTokenSource m) => MonadCommandSource m where
  getCommand :: m (Maybe Command)
