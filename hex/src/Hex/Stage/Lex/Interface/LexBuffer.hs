module Hex.Stage.Lex.Interface.LexBuffer where

import Hex.Common.Codes qualified as Code
import Hex.Stage.Categorise.Interface.CharSource qualified as Cat
import Hex.Stage.Lex.Interface.Extract
import Hexlude

data LexBuffer = LexBuffer
  { bufferCharSource :: Cat.LoadedCharSource,
    bufferLexTokens :: Seq LexToken,
    bufferLexState :: LexState
  }
  deriving stock (Show, Generic)

newLexBuffer :: Maybe Code.CharCode -> NonEmpty ByteString -> LexBuffer
newLexBuffer mayEndLineChar sourceLines =
  LexBuffer
    { bufferCharSource = Cat.newCharSource mayEndLineChar sourceLines,
      bufferLexTokens = mempty,
      bufferLexState = LineBegin
    }
