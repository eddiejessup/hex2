module Hex.Stage.Lex.Interface.CharSource where

import Hexlude
import Hex.Stage.Lex.Interface.Extract

data CharSource = CharSource
  { sourcePath :: Maybe FilePath,
    sourceChars :: ByteString,
    sourceLexTokens :: Seq LexToken,
    sourceLexState :: LexState
  }
  deriving stock (Show, Generic)

newCharSource :: ByteString -> CharSource
newCharSource sourceChars =
  CharSource
    { sourcePath = Nothing,
      sourceChars,
      sourceLexTokens = mempty,
      sourceLexState = LineBegin
    }
