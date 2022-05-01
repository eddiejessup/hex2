module Hex.Stage.Evaluate.Interface where

import Hex.Stage.Evaluate.Interface.AST.Command (Command)
import Hexlude
import Hex.Stage.Lex.Interface.CharSource (CharSource)
import qualified Hex.Stage.Lex.Interface.Extract as Lex

class Monad m => MonadEvaluated m where
  getCommand :: m Command

  getSource :: m CharSource

  putSource :: CharSource -> m ()

  insertLexTokenToSource :: Lex.LexToken -> m ()

  insertLexTokensToSource :: Seq Lex.LexToken -> m ()
