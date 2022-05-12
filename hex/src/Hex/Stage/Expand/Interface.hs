module Hex.Stage.Expand.Interface where

import Formatting qualified as F
import Hex.Common.HexState.Interface.Resolve qualified as Res
import Hex.Common.HexState.Interface.Resolve.PrimitiveToken qualified as PT
import Hex.Common.HexState.Interface.Resolve.SyntaxToken qualified as ST
import Hex.Stage.Lex.Interface.Extract qualified as Lex
import Hex.Stage.Parse.Interface.AST.SyntaxCommand qualified as AST
import Hexlude

data ExpansionError
  = MacroArgumentSubstitutionError ST.ParameterNumber AST.MacroArgumentList
  | EndOfInputWhileSkipping
  deriving stock (Show, Generic)

fmtExpansionError :: Fmt ExpansionError
fmtExpansionError = F.later $ \case
  -- ExpansionResolutionError e -> "Failed to resolve while expanding: " <> F.bformat Res.fmtResolutionError e
  MacroArgumentSubstitutionError argIx args ->
    "Failed to find parameter value in macro call, at index " <> F.bformat F.shown argIx <> " of arguments " <> F.bformat F.shown args
  EndOfInputWhileSkipping ->
    "Got to end of input while skipping"

data IfState
  = InUnskippedElseBlock
  | InUnskippedPreElseBlock
  deriving stock (Show, Generic)

class Monad m => MonadPrimTokenSource m where
  getTokenInhibited :: m (Maybe Lex.LexToken)

  getResolvedToken :: m (Maybe (Lex.LexToken, Res.ResolvedToken))

  getPrimitiveToken :: m (Maybe (Lex.LexToken, PT.PrimitiveToken))

  pushIfState :: IfState -> m ()

getResolvedTokenErrorEOF :: (MonadPrimTokenSource m, MonadError e m) => e -> m Res.ResolvedToken
getResolvedTokenErrorEOF e = do
  (_lt, rt) <- nothingToError getResolvedToken e
  pure rt
