module Hex.Evaluate.MonadEvaluated.Interface where

import Hex.Evaluate.AST.Command (Command)
import Hex.Lex.Types qualified as H.Lex
import Hex.Parse.CharSource qualified as H.Par.ChrSrc
import Hexlude

class Monad m => MonadEvaluated m where
  parseCommand :: m Command

  getStream :: m H.Par.ChrSrc.CharSource

  putStream :: H.Par.ChrSrc.CharSource -> m ()

  insertLexTokenToStream :: H.Lex.LexToken -> m ()

  insertLexTokensToStream :: Seq H.Lex.LexToken -> m ()
