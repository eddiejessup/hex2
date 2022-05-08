module Hex.Stage.Expand.Interface where

import Data.Sequence qualified as Seq
import Formatting qualified as F
import Hex.Common.HexState.Interface.Resolve.PrimitiveToken qualified as PT
import Hex.Common.HexState.Interface.Resolve.SyntaxToken qualified as ST
import Hex.Common.Parse qualified as Par
import Hex.Stage.Lex.Interface.Extract qualified as Lex
import Hex.Stage.Resolve.Interface qualified as Res
import Hexlude

newtype MacroArgument = MacroArgument {unMacroArgument :: ST.InhibitedBalancedText}
  deriving stock (Show, Eq, Generic)

newtype MacroArgumentList = MacroArgumentList {unMacroArgumentList :: Seq MacroArgument}
  deriving stock (Show, Eq, Generic)

lookupArg :: ST.ParameterNumber -> MacroArgumentList -> Maybe MacroArgument
lookupArg p argList =
  let argIx = fromEnum p.unParameterNumber - 1
   in (argList.unMacroArgumentList) Seq.!? argIx

data ExpansionError
  = ExpansionResolutionError Res.ResolutionError
  | MacroArgumentSubstitutionError ST.ParameterNumber MacroArgumentList
  | ExpansionParsingError Par.ParsingError
  deriving stock (Generic, Show)

fmtExpansionError :: Fmt ExpansionError
fmtExpansionError = F.later $ \case
  ExpansionResolutionError e -> "Failed to resolve while expanding: " <> F.bformat Res.fmtResolutionError e
  MacroArgumentSubstitutionError argIx args ->
    "Failed to find parameter value in macro call, at index " <> F.bformat F.shown argIx <> " of arguments " <> F.bformat F.shown args
  ExpansionParsingError e -> "Parse failed while expanding: " <> F.bformat Par.fmtParsingError e

class MonadPrimTokenSource m where
  getTokenInhibited :: m (Maybe Lex.LexToken)

  getPrimitiveToken :: m (Maybe (Lex.LexToken, PT.PrimitiveToken))
