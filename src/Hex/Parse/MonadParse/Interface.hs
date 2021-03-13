module Hex.Parse.MonadParse.Interface where

import Hex.Lex.Types qualified as H.Lex
import Hex.Parse.AST (Command)
import Hex.Parse.CharSource qualified as H.Par.ChrSrc
import Hexlude

class Monad m => MonadParse m where
  parseCommand :: m Command

  getStream :: m H.Par.ChrSrc.CharSource

  putStream :: H.Par.ChrSrc.CharSource -> m ()

  insertLexTokenToStream :: H.Lex.LexToken -> m ()

  insertLexTokensToStream :: Seq H.Lex.LexToken -> m ()
