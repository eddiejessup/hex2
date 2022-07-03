module Hex.Common.HexState.Interface.Resolve where

import Hexlude
import Hex.Common.HexState.Interface.Resolve.PrimitiveToken
import Hex.Common.HexState.Interface.Resolve.ExpandableToken
import qualified Hex.Common.Codes as Code
import qualified Hex.Stage.Lex.Interface.Extract as Lex
import qualified Formatting as F

-- Symbol to be resolved.
data ControlSymbol
  = ActiveCharacterSymbol Code.CharCode
  | ControlSequenceSymbol Lex.ControlSequence
  deriving stock (Show, Eq, Ord, Generic)

fmtControlSymbol :: Fmt ControlSymbol
fmtControlSymbol = F.later $ \case
  ActiveCharacterSymbol c -> "Active '" <> F.bformat Code.fmtCharCode c <> "'"
  ControlSequenceSymbol cs -> F.bformat Lex.fmtControlSequence cs

-- The result of resolving a symbol.
data ResolvedToken
  = ExpansionCommandHeadToken ExpansionCommandHeadToken
  | PrimitiveToken PrimitiveToken
  deriving stock (Show, Eq, Generic)

fmtResolvedToken :: Fmt ResolvedToken
fmtResolvedToken = F.later $ \case
  ExpansionCommandHeadToken st -> F.bformat fmtExpansionCommandHeadTokenType st
  PrimitiveToken pt -> F.bformat fmtPrimitiveToken pt

fmtExpansionCommandHeadTokenType :: Fmt ExpansionCommandHeadToken
fmtExpansionCommandHeadTokenType = F.later $ \case
  MacroTok _macroDefinition ->
    "CallMacro"
  ConditionTok (ConditionHeadTok conditionHeadTok) -> case conditionHeadTok of
    IfIntPairTestTok -> "IfIntPairTest"
    IfLengthPairTestTok -> "IfLengthPairTest"
    IfIntOddTok -> "IfIntOdd"
    IfInModeTok _modeAttribute ->
      "IfInMode"
    IfTokenAttributesEqualTok _tokenAttribute ->
      "IfTokenAttributesEqual"
    IfTokensEqualTok -> "IfTokensEqual"
    IfBoxRegisterIsTok _boxRegisterAttribute ->
      "IfBoxRegisterIs"
    IfInputEndedTok -> "IfInputEnded"
    IfConstTok True -> "IfTrue"
    IfConstTok False -> "IfFalse"
    CaseTok -> "CaseTok"
  ConditionTok (ConditionBodyTok conditionBodyTok) -> case conditionBodyTok of
    Else -> "Else"
    Or -> "Or"
    EndIf -> "EndIf"
  NumberTok ->
    "Number"
  RomanNumeralTok ->
    "RomanNumeral"
  StringTok ->
    "String"
  JobNameTok ->
    "JobName"
  FontNameTok ->
    "FontName"
  MeaningTok ->
    "Meaning"
  CSNameTok ->
    "CSName"
  ExpandAfterTok ->
    "ExpandAfter"
  NoExpandTok ->
    "NoExpand"
  MarkRegisterTok _markRegister ->
    "MarkRegister"
  InputTok ->
    "Input"
  EndInputTok ->
    "EndInput"
  TheTok ->
    "The"
  ChangeCaseTok vDirection ->
    "ChangeCase" <> F.bformat F.shown vDirection

type SymbolMap = Map ControlSymbol ResolvedToken
