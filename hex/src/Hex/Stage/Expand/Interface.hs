module Hex.Stage.Expand.Interface where

import Formatting qualified as F
import Hex.Common.HexState.Interface.Resolve qualified as Res
import Hex.Common.HexState.Interface.Resolve.PrimitiveToken qualified as PT
import Hex.Common.HexState.Interface.Resolve.ExpandableToken qualified as ST
import Hex.Stage.Lex.Interface.Extract qualified as Lex
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

class Monad m => MonadPrimTokenSource m where
  getTokenInhibited :: m (Maybe Lex.LexToken)

  getResolvedToken :: m (Maybe (Lex.LexToken, Res.ResolvedToken))

  getPrimitiveToken :: m (Maybe (Lex.LexToken, PT.PrimitiveToken))

  pushConditionState :: ConditionState -> m ()

  popConditionState :: m (Maybe ConditionState)

  peekConditionState :: m (Maybe ConditionState)

getResolvedTokenErrorEOF :: (MonadPrimTokenSource m, MonadError e m) => e -> m Res.ResolvedToken
getResolvedTokenErrorEOF e = do
  (_lt, rt) <- nothingToError getResolvedToken e
  pure rt
