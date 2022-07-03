module Hex.Common.HexState.Interface.Resolve.SyntaxToken where

import ASCII qualified
import Hex.Common.HexState.Interface.Resolve.PrimitiveToken
import Hex.Common.Quantity qualified as Q
import Hex.Stage.Lex.Interface.Extract qualified as Lex
import Hexlude

-- Like a balanced-text, except with a proof that it contains no braces.
newtype ParameterText = ParameterText (Seq Lex.LexToken)
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
  = MacroTextLexToken Lex.LexToken -- A 'normal' token.
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

data SyntaxCommandHeadToken
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
  | ChangeCaseTok Q.VDirection -- \uppercase, \lowercase
  deriving stock (Show, Eq, Generic)
