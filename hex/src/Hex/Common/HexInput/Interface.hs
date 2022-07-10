module Hex.Common.HexInput.Interface where

import Formatting qualified as F
import Hex.Common.HexInput.Interface.CharSource
import Hex.Common.Token.Lexed qualified as LT
import Hexlude

class Monad m => MonadHexInput m where
  endCurrentLine :: m ()

  sourceIsFinished :: m Bool

  getSource :: m LoadedCharSource

  putSource :: LoadedCharSource -> m ()

  insertLexToken :: LT.LexToken -> m ()

  insertLexTokens :: Seq LT.LexToken -> m ()

  getNextLexToken :: m (Maybe LT.LexToken)

data LexError
  = TerminalEscapeCharacter
  | InvalidCharacter
  | NoMoreLines
  deriving stock (Show, Eq, Generic)

fmtLexError :: Fmt LexError
fmtLexError = F.shown
