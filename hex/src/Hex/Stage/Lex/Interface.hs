module Hex.Stage.Lex.Interface where

import Hex.Stage.Lex.Interface.CharSource qualified as Lex
import Hex.Stage.Lex.Interface.Extract qualified as Lex
import Hexlude

class Monad m => MonadLexTokenSource m where
  getLexToken :: m (Maybe Lex.LexToken)

  insertLexTokenToSource :: Lex.LexToken -> m ()

  insertLexTokensToSource :: Seq Lex.LexToken -> m ()

  getSource :: m Lex.CharSource

  putSource :: Lex.CharSource -> m ()
