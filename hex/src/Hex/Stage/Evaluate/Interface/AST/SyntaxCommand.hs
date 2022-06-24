module Hex.Stage.Evaluate.Interface.AST.SyntaxCommand where

import Hex.Common.HexState.Interface.Resolve.PrimitiveToken qualified as PT
import Hex.Common.HexState.Interface.Resolve.SyntaxToken qualified as PT.Syn
import Hex.Common.HexState.Interface.Resolve.SyntaxToken qualified as ST
import Hex.Common.Quantity qualified as Q
import Hex.Stage.Evaluate.Interface.AST.Quantity qualified as Eval
import Hex.Stage.Lex.Interface.Extract qualified as Lex
import Hex.Stage.Parse.Interface.AST.Quantity qualified as Uneval
import Hex.Stage.Parse.Interface.AST.SyntaxCommand qualified as Uneval
import Hexlude

data SyntaxCommand
  = CallMacro ST.MacroDefinition Uneval.MacroArgumentList
  | ApplyConditionHead ConditionOutcome
  | ApplyConditionBody ST.ConditionBodyTok
  | RenderNumber Q.HexInt
  | RenderRomanNumeral Q.HexInt
  | RenderTokenAsTokens Lex.LexToken
  | RenderJobName
  | RenderFontName Uneval.FontRef
  | RenderTokenMeaning Lex.LexToken
  | ParseControlSequence ByteString
  | ExpandAfter Lex.LexToken Lex.LexToken
  | NoExpand Lex.LexToken
  | GetMarkRegister ST.MarkRegister
  | OpenInputFile Q.HexFilePath
  | EndInputFile
  | RenderInternalQuantity Eval.InternalQuantity
  | ChangeCase Q.VDirection ST.InhibitedBalancedText
  deriving stock (Show, Eq, Generic)

data ConditionOutcome = IfConditionOutcome IfOutcome | CaseConditionOutcome Q.HexInt
  deriving stock (Show, Eq, Generic)

data IfOutcome
  = SkipPreElseBlock
  | SkipElseBlock
  deriving stock (Show, Eq, Generic)

data IfConditionHead
  = IfIntPairTest Q.HexInt Ordering Q.HexInt -- \ifnum
  | IfLengthPairTest Q.Length Ordering Q.Length -- \ifdim
  | IfIntOdd Q.HexInt -- \ifodd
  | IfInMode PT.ModeAttribute -- \ifvmode, \ifhmode, \ifmmode, \ifinner
  | IfTokenAttributesEqual PT.Syn.TokenAttribute Lex.LexToken Lex.LexToken -- \if, \ifcat
  | IfTokensEqual Lex.LexToken Lex.LexToken -- \ifx
  | IfBoxRegisterIs PT.BoxRegisterAttribute Q.HexInt -- \ifvoid, \ifhbox, \ifvbox
  | IfInputEnded Q.HexInt -- \ifeof
  | IfConst Bool -- \iftrue, \iffalse
  deriving stock (Show, Eq, Generic)
