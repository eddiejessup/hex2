module Hex.Stage.Parse.Interface where

import Hex.Stage.Parse.Interface.AST.Command (Command)

class MonadParse m where
  parseCommand :: m Command

  -- insertLexTokenToSource :: Lex.LexToken -> m ()

  -- insertLexTokensToSource :: Seq Lex.LexToken -> m ()

  -- getSource :: m CharSource

  -- putSource :: CharSource -> m ()
