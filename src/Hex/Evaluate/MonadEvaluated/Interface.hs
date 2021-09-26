module Hex.Evaluate.MonadEvaluated.Interface where

import Hex.Syntax.Command qualified as H.Syn
import Hex.Syntax.Common qualified as H.Syn
import Hex.Lex.Types qualified as H.Lex
import Hex.Parse.CharSource qualified as H.Par.ChrSrc
import Hexlude

class Monad m => MonadEvaluated m where
  parseCommand :: m (H.Syn.Command 'H.Syn.Evaluated)

  getStream :: m H.Par.ChrSrc.CharSource

  putStream :: H.Par.ChrSrc.CharSource -> m ()

  insertLexTokenToStream :: H.Lex.LexToken -> m ()

  insertLexTokensToStream :: Seq H.Lex.LexToken -> m ()
