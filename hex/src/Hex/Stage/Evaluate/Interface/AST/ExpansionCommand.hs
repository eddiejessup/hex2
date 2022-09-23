module Hex.Stage.Evaluate.Interface.AST.ExpansionCommand where

import Hex.Common.HexState.Interface.Mode qualified as HSt.Mode
import Hex.Common.HexState.Interface.TokenList qualified as HSt.TL
import Hex.Common.Quantity qualified as Q
import Hex.Common.Token.Lexed qualified as LT
import Hex.Common.Token.Resolved.Expandable qualified as PT.Syn
import Hex.Common.Token.Resolved.Expandable qualified as ST
import Hex.Common.Token.Resolved.Primitive qualified as PT
import Hex.Stage.Evaluate.Interface.AST.Quantity qualified as Eval
import Hex.Stage.Parse.Interface.AST.ExpansionCommand qualified as Uneval
import Hex.Stage.Parse.Interface.AST.Quantity qualified as Uneval
import Hexlude

data ExpansionCommand
  = CallMacro ST.MacroDefinition Uneval.MacroArgumentList
  | ApplyConditionHead ConditionOutcome
  | ApplyConditionBody ST.ConditionBodyTok
  | RenderNumber Q.HexInt
  | RenderRomanNumeral Q.HexInt
  | RenderTokenAsTokens LT.LexToken
  | RenderJobName
  | RenderFontName Uneval.FontRef
  | RenderTokenMeaning LT.LexToken
  | ParseControlSequence LT.ControlSequence
  | ExpandAfter LT.LexToken LT.LexToken
  | NoExpand LT.LexToken
  | GetMarkRegister ST.MarkRegister
  | ReadFile HexFilePath
  | EndInputFile
  | RenderInternalQuantity Eval.InternalQuantity
  | ChangeCase VDirection HSt.TL.BalancedText
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
  | IfInMode HSt.Mode.ModeAttribute -- \ifvmode, \ifhmode, \ifmmode, \ifinner
  | IfTokenAttributesEqual PT.Syn.TokenAttribute LT.LexToken LT.LexToken -- \if, \ifcat
  | IfTokensEqual LT.LexToken LT.LexToken -- \ifx
  | IfBoxRegisterIs PT.BoxRegisterAttribute Q.HexInt -- \ifvoid, \ifhbox, \ifvbox
  | IfInputEnded Q.FourBitInt -- \ifeof
  | IfConst Bool -- \iftrue, \iffalse
  deriving stock (Show, Eq, Generic)
