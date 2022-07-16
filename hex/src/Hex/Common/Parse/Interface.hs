module Hex.Common.Parse.Interface where

-- Interface for parsing primitive-token streams.

import Formatting qualified as F
import Hex.Capability.Log.Interface qualified as Log
import Hex.Common.Token.Lexed qualified as LT
import Hex.Common.Token.Resolved.Primitive qualified as PT
import Hexlude

data ParseUnexpectedError
  = ParseDefaultFailure
  | ParseExplicitFailure Text
  | SawUnexpectedPrimitiveToken UnexpectedPrimitiveToken
  | SawUnexpectedLexToken UnexpectedLexToken
  deriving stock (Show, Eq, Generic)

fmtParseUnexpectedErrorCause :: Fmt ParseUnexpectedError
fmtParseUnexpectedErrorCause =
  F.shown

data UnexpectedPrimitiveToken = UnexpectedPrimitiveToken {saw :: PT.PrimitiveToken, expected :: Text}
  deriving stock (Show, Eq, Generic)

data UnexpectedLexToken = UnexpectedLexToken {saw :: LT.LexToken, expected :: Text}
  deriving stock (Show, Eq, Generic)

class (Monad m, Alternative m, MonadPlus m, Log.MonadHexLog m) => MonadPrimTokenParse m where
  parseError :: ParseUnexpectedError -> m a

  satisfyThenExpanding :: ((LT.LexToken, PT.PrimitiveToken) -> Maybe a) -> m a

  satisfyThenInhibited :: (LT.LexToken -> Maybe a) -> m a

  try :: m a -> m a

parseFailure :: MonadPrimTokenParse m => Text -> m a
parseFailure msg = do
  Log.debugLog $ "Parse failure: " <> msg
  parseError $ ParseExplicitFailure msg
