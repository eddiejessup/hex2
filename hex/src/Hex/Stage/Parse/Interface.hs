module Hex.Stage.Parse.Interface where

import Hex.Stage.Parse.Interface.AST.Command (Command)
import Hexlude

class MonadCommandSource m where
  getCommand :: m (Maybe Command)

  -- insertLexTokenToSource :: Lex.LexToken -> m ()

  -- insertLexTokensToSource :: Seq Lex.LexToken -> m ()

  -- getSource :: m CharSource

  -- putSource :: CharSource -> m ()
