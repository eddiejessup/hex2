module Hex.Parse.MonadParse.Interface where

import Hex.Lex.Types qualified as H.Lex
import Hex.Syntax.Command qualified as H.Syn
import Hex.Syntax.Common qualified as H.Syn
import Hex.Parse.CharSource qualified as H.Par.ChrSrc
import Hexlude

class Monad m => MonadParse m where
  parseCommand :: m (H.Syn.Command 'H.Syn.Parsed)

  getStream :: m H.Par.ChrSrc.CharSource

  putStream :: H.Par.ChrSrc.CharSource -> m ()

  insertLexTokenToStream :: H.Lex.LexToken -> m ()

  insertLexTokensToStream :: Seq H.Lex.LexToken -> m ()
