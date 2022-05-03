module Hex.Common.HexState.Interface.Resolve.SyntaxToken where

import Hex.Common.Quantity qualified as Q
import Hex.Common.HexState.Interface.Resolve.PrimitiveToken
import Hexlude
import qualified Hex.Stage.Lex.Interface.Extract as Lex

newtype InhibitedBalancedText = InhibitedBalancedText (Seq Lex.LexToken)
  deriving stock (Show, Generic)
  deriving newtype (Eq, Semigroup, Monoid)

newtype ExpandedBalancedText = ExpandedBalancedText (Seq PrimitiveToken)
  deriving stock (Show, Generic)
  deriving newtype (Eq, Semigroup, Monoid)

-- Like a balanced-text, except with a proof that it contains no braces.
newtype ParameterText = ParameterText (Seq Lex.LexToken)
  deriving stock (Show, Generic)
  deriving newtype (Eq)

data MacroReplacementText
  = ExpandedReplacementText ExpandedBalancedText
  | InhibitedReplacementText InhibitedBalancedText
  deriving stock (Show, Eq, Generic)

data MacroDefinition = MacroDefinition
  { paramText :: ParameterText,
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
  deriving stock (Show, Eq, Generic)

data ConditionTok
  = ConditionHeadTok ConditionHeadTok
  | ConditionBodyTok ConditionBodyTok
  deriving stock (Show, Eq, Generic)

data ConditionHeadTok
  = IfHexIntPairTestTok -- \ifnum
  | IfLengthPairTestTok -- \ifdim
  | IfHexIntOddTok -- \ifodd
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
