module Hex.Stage.Lex.Interface where

import Hex.Stage.Lex.Interface.Extract qualified as Lex
import Hex.Stage.Lex.Interface.LexBuffer (LexBuffer)
import Hexlude

class Monad m => MonadLexTokenSource m where
  getLexToken :: m (Maybe Lex.LexToken)

  insertLexTokenToSource :: Lex.LexToken -> m ()

  insertLexTokensToSource :: Seq Lex.LexToken -> m ()

  getSource :: m LexBuffer

  putSource :: LexBuffer -> m ()

instance MonadLexTokenSource m => MonadLexTokenSource (StateT s m) where
  getLexToken = lift getLexToken

  insertLexTokenToSource x = lift $ insertLexTokenToSource x

  insertLexTokensToSource x = lift $ insertLexTokensToSource x

  getSource = lift getSource

  putSource x = lift $ putSource x
