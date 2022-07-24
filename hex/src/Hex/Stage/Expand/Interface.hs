{-# LANGUAGE TemplateHaskell #-}

module Hex.Stage.Expand.Interface where

import Formatting qualified as F
import Hex.Common.Token.Lexed qualified as LT
import Hex.Common.Token.Resolved.Expandable qualified as ST
import Hex.Common.Token.Resolved.Primitive qualified as PT
import Hex.Stage.Parse.Interface.AST.ExpansionCommand qualified as AST
import Hexlude

data ExpansionError
  = MacroArgumentSubstitutionError ST.ParameterNumber AST.MacroArgumentList
  | EndOfInputWhileSkipping
  | UnexpectedConditionBodyToken ST.ConditionBodyTok
  deriving stock (Show, Generic)

fmtExpansionError :: Fmt ExpansionError
fmtExpansionError = F.later $ \case
  MacroArgumentSubstitutionError argIx args ->
    "Failed to find parameter value in macro call, at index " <> F.bformat F.shown argIx <> " of arguments " <> F.bformat F.shown args
  EndOfInputWhileSkipping ->
    "Got to end of input while skipping"
  UnexpectedConditionBodyToken tok ->
    "Got unexpected token when not in condition-body: " <> F.bformat F.shown tok

data IfState
  = InSelectedElseIfBlock
  | InSelectedPreElseIfBlock
  deriving stock (Show, Generic)

data CaseState
  = InSelectedOrCaseBlock
  | InSelectedElseCaseBlock
  deriving stock (Show, Generic)

data ConditionState
  = IfConditionState IfState
  | CaseConditionState CaseState
  deriving stock (Show, Generic)

newtype ConditionStates = ConditionStates {unConditionStates :: [ConditionState]}
  deriving stock (Show, Generic)

newConditionStates :: ConditionStates
newConditionStates = ConditionStates []

data ParsingError
  = EndOfInputParsingError
  | UnexpectedParsingError ParseUnexpectedError
  deriving stock (Show, Eq, Generic)

fmtParsingError :: Fmt ParsingError
fmtParsingError = F.later $ \case
  EndOfInputParsingError -> "End of input"
  UnexpectedParsingError e -> F.bformat fmtParseUnexpectedErrorCause e

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

data PrimTokenSource :: Effect where
  GetPrimitiveToken :: PrimTokenSource m (Maybe (LT.LexToken, PT.PrimitiveToken))
  SatisfyThenExpanding :: ((LT.LexToken, PT.PrimitiveToken) -> Maybe a) -> PrimTokenSource m a
  SatisfyThenInhibited :: (LT.LexToken -> Maybe a) -> PrimTokenSource m a
  TryParse :: m a -> PrimTokenSource m a
  FailParse :: ParseUnexpectedError -> PrimTokenSource m a
  PushConditionState :: ConditionState -> PrimTokenSource m ()
  PopConditionState :: PrimTokenSource m (Maybe ConditionState)
  PeekConditionState :: PrimTokenSource m (Maybe ConditionState)

makeEffect ''PrimTokenSource

parseFail :: PrimTokenSource :> es => Text -> Eff es a
parseFail = failParse . ParseExplicitFailure
