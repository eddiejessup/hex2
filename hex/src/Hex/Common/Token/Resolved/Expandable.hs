module Hex.Common.Token.Resolved.Expandable where

import ASCII qualified
import Formatting qualified as F
import Hex.Common.Token.Lexed qualified as LT
import Hex.Common.Token.Resolved.Primitive
import Hexlude

-- Like a balanced-text, except with a proof that it contains no braces.
newtype ParameterText = ParameterText (Seq LT.LexToken)
  deriving stock (Show, Generic)
  deriving newtype (Eq)

-- | The form of parameters that are part of a macro definition.
data MacroParameterSpecification = MacroParameterSpecification
  { preParameterText :: ParameterText,
    parameterDelimiterTexts :: Seq ParameterText
  }
  deriving stock (Show, Eq, Generic)

data MacroReplacementText
  = ExpandedMacroReplacementText -- Not implemented
  | InhibitedMacroReplacementText InhibitedReplacementText
  deriving stock (Show, Eq, Generic)

newtype InhibitedReplacementText = InhibitedReplacementText {unInhibitedReplacementText :: Seq MacroTextToken}
  deriving stock (Show, Eq, Generic)

-- A token in a macro template.
data MacroTextToken
  = MacroTextLexToken LT.LexToken -- A 'normal' token.
  | MacroTextParamToken ParameterNumber
  deriving stock (Show, Eq, Generic)

newtype ParameterNumber = ParameterNumber {unParameterNumber :: ASCII.Digit}
  deriving stock (Show, Eq, Generic)

data MacroDefinition = MacroDefinition
  { parameterSpecification :: MacroParameterSpecification,
    replacementText :: MacroReplacementText,
    long, outer :: Bool
  }
  deriving stock (Show, Eq, Generic)

data TokenAttribute
  = CharCodeAttribute -- \if
  | CatCodeAttribute -- \ifcat
  deriving stock (Show, Eq, Generic)

data MarkRegister
  = TopMark -- \topmark
  | FirstMark -- \firstmark
  | BottomMark -- \botmark
  | SplitFirstMark -- \splitfirstmark
  | SplitBotMark -- \splitbotmark
  deriving stock (Show, Eq, Generic)

data ConditionTok
  = ConditionHeadTok ConditionHeadTok
  | ConditionBodyTok ConditionBodyTok
  deriving stock (Show, Eq, Generic)

data ConditionHeadTok
  = IfIntPairTestTok -- \ifnum
  | IfLengthPairTestTok -- \ifdim
  | IfIntOddTok -- \ifodd
  | IfInModeTok ModeAttribute -- \ifvmode, \ifhmode, \ifmmode, \ifinner
  | IfTokenAttributesEqualTok TokenAttribute
  | IfTokensEqualTok -- \ifx
  | IfBoxRegisterIsTok BoxRegisterAttribute -- \ifvoid, \ifhbox, \ifvbox
  | IfInputEndedTok -- \ifeof
  | IfConstTok Bool -- \iftrue, \iffalse
  | CaseTok -- \ifcase
  deriving stock (Show, Eq, Generic)

data ConditionBodyTok
  = Else -- \else
  | Or -- \or
  | EndIf -- \fi
  deriving stock (Show, Eq, Generic)

data ExpansionCommandHeadToken
  = MacroTok MacroDefinition
  | ConditionTok ConditionTok
  | NumberTok -- \number
  | RomanNumeralTok -- \romannumeral
  | StringTok -- \string
  | JobNameTok -- \jobname
  | FontNameTok -- \fontname
  | MeaningTok -- \meaning
  | CSNameTok -- \csname
  | ExpandAfterTok -- \expandafter
  | NoExpandTok -- \noexpand
  | MarkRegisterTok MarkRegister
  | InputTok -- \input
  | EndInputTok -- \endinput
  | TheTok -- \the
  | ChangeCaseTok VDirection -- \uppercase, \lowercase
  deriving stock (Show, Eq, Generic)

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
