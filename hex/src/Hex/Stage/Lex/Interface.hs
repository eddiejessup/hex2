module Hex.Stage.Lex.Interface where

import Hexlude
import qualified Formatting as F
import qualified Hex.Common.Token.Lexed as LT

data LexError
  = TerminalEscapeCharacter
  | InvalidCharacter
  deriving stock (Show, Eq, Generic)

fmtLexError :: Fmt LexError
fmtLexError = F.shown

class Monad m => MonadLexTokenSource m where
  getLexToken :: m (Maybe LT.LexToken)

instance MonadLexTokenSource m => MonadLexTokenSource (StateT s m) where
  getLexToken = lift getLexToken
