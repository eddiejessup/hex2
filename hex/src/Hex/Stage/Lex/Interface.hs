module Hex.Stage.Lex.Interface where

import Hexlude
import qualified Hex.Stage.Lex.Interface.Extract as Lex
import qualified Hex.Stage.Lex.Interface.CharSource as Lex

class Monad m => MonadLexTokenSource m where
  getLexToken :: m (Maybe Lex.LexToken)

  insertLexTokenToSource :: Lex.LexToken -> m ()

  insertLexTokensToSource :: Seq Lex.LexToken -> m ()

  getSource :: m Lex.CharSource

  putSource :: Lex.CharSource -> m ()
