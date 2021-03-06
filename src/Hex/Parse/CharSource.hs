module Hex.Parse.CharSource where

import Protolude
import Path qualified
import Hex.Lex.Types qualified as H.Lex

data CharSource = CharSource
  { sourcePath :: Maybe (Path.Path Path.Abs Path.File),
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
