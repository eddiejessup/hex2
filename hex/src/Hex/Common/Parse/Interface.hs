{-# LANGUAGE TemplateHaskell #-}

module Hex.Common.Parse.Interface where

-- Interface for parsing primitive-token streams.

import Formatting qualified as F
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

data PrimTokenParse :: Effect where
  SatisfyThenExpanding :: ((LT.LexToken, PT.PrimitiveToken) -> Maybe a) -> PrimTokenParse m a
  SatisfyThenInhibited :: (LT.LexToken -> Maybe a) -> PrimTokenParse m a
  TryParse :: m a -> PrimTokenParse m a
  FailParse :: ParseUnexpectedError -> PrimTokenParse m a

makeEffect ''PrimTokenParse

-- parseFail :: Text -> PrimTokenParse m a
parseFail :: PrimTokenParse :> es => Text -> Eff es a
parseFail = failParse . ParseExplicitFailure
