module Hex.Common.Parse.Interface where

-- Interface for parsing primitive-token streams.

import Formatting qualified as F
import Hex.Capability.Log.Interface qualified as Log
import Hex.Common.Token.Lexed qualified as LT
import Hex.Common.Token.Resolved.Primitive qualified as PT
import Hexlude

data ParsingError
  = EndOfInputParsingError
  | UnexpectedParsingError ParseUnexpectedError
  deriving stock (Show, Eq, Generic)

fmtParsingError :: Fmt ParsingError
fmtParsingError = F.later $ \case
  EndOfInputParsingError -> "End of input"
  UnexpectedParsingError e -> F.bformat fmtParseUnexpectedError e

data ParseUnexpectedError = ParseUnexpectedError
  { err :: ParseUnexpectedErrorCause
  }
  deriving stock (Show, Eq, Generic)

fmtParseUnexpectedError :: Fmt ParseUnexpectedError
fmtParseUnexpectedError =
  "ParseUnexpectedError: " |%| F.accessed (.err) F.shown

data ParseUnexpectedErrorCause
  = ParseDefaultFailure
  | ParseExplicitFailure Text
  | SawUnexpectedPrimitiveToken UnexpectedPrimitiveToken
  | SawUnexpectedLexToken UnexpectedLexToken
  deriving stock (Show, Eq, Generic)

data UnexpectedPrimitiveToken = UnexpectedPrimitiveToken {saw :: PT.PrimitiveToken, expected :: Text}
  deriving stock (Show, Eq, Generic)

data UnexpectedLexToken = UnexpectedLexToken {saw :: LT.LexToken, expected :: Text}
  deriving stock (Show, Eq, Generic)

-- I want a single class to require when I write my parsers that consume primitive tokens.
-- For this I need more than just MonadPrimTokenSource, because I want to write a backtracking parser.
-- For this, I need to be able to get at the underlying char-source, so I can reset the state.
-- So I introduce this class, which I will implement for 'ParseT m', so long as `m` has the necessary
-- instances to:
-- - Get primitive tokens (i.e. MonadPrimTokenSource)
-- - Get the underlying stream (i.e. MonadLexTokenSource)

-- This class implements a method to get an individual primitive token,
-- but it is *different* from the 'MonadPrimTokenSource' implementation:
-- Because we need this alternative/monadplus monoid behaviour, to try different parsers,
-- we need end-of-input to raise an error. The '..source' implementation returns a maybe, so we would have to do lots of work to cast that back as an error.
-- In the 'parser' case, we want to raise an error on end-of-input.
-- We just add the 'Monad m, Alternative m' constraints because I know any reasonable use is going to require this,
-- and it saves typing an extra constraint at the use-site in such cases.
class (Monad m, Alternative m, MonadPlus m, Log.MonadHexLog m) => MonadPrimTokenParse m where
  parseError :: ParseUnexpectedErrorCause -> m a

  satisfyThenExpanding :: ((LT.LexToken, PT.PrimitiveToken) -> Maybe a) -> m a

  satisfyThenInhibited :: (LT.LexToken -> Maybe a) -> m a

  try :: m a -> m a

parseFailure :: MonadPrimTokenParse m => Text -> m a
parseFailure msg = do
  Log.log $ "Parse failure: " <> msg
  parseError $ ParseExplicitFailure msg
