module Hex.Parse.CharSource where

import Hex.Lex.Types qualified as H.Lex
import Hexlude

data CharSource = CharSource
  { sourcePath :: Maybe FilePath,
    sourceChars :: ByteString,
    sourceLexTokens :: Seq H.Lex.LexToken,
    sourceLexState :: H.Lex.LexState
  }
  deriving stock (Generic)

newCharSource :: ByteString -> CharSource
newCharSource sourceChars =
  CharSource
    { sourcePath = Nothing,
      sourceChars,
      sourceLexTokens = mempty,
      sourceLexState = H.Lex.LineBegin
    }
