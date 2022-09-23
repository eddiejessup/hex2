module Hex.Stage.Parse.Interface.AST.ExpansionCommand where

import Data.Sequence qualified as Seq
import Hex.Common.HexState.Interface.Mode qualified as HSt.Mode
import Hex.Common.HexState.Interface.TokenList qualified as HSt.TL
import Hex.Common.Token.Lexed qualified as LT
import Hex.Common.Token.Resolved.Expandable qualified as PT.Syn
import Hex.Common.Token.Resolved.Expandable qualified as ST
import Hex.Common.Token.Resolved.Primitive qualified as PT
import Hex.Stage.Parse.Interface.AST.Command (InternalQuantity)
import Hex.Stage.Parse.Interface.AST.Quantity
import Hexlude

data IfConditionHead
  = IfIntPairTest HexInt Ordering HexInt -- \ifnum
  | IfLengthPairTest Length Ordering Length -- \ifdim
  | IfIntOdd HexInt -- \ifodd
  | IfInMode HSt.Mode.ModeAttribute -- \ifvmode, \ifhmode, \ifmmode, \ifinner
  | IfTokenAttributesEqual PT.Syn.TokenAttribute LT.LexToken LT.LexToken -- \if, \ifcat
  | IfTokensEqual LT.LexToken LT.LexToken -- \ifx
  | IfBoxRegisterIs PT.BoxRegisterAttribute HexInt -- \ifvoid, \ifhbox, \ifvbox
  | IfInputEnded HexInt -- \ifeof
  | IfConst Bool -- \iftrue, \iffalse
  deriving stock (Show, Eq, Generic)

data ConditionHead = IfConditionHead IfConditionHead | CaseConditionHead HexInt
  deriving stock (Show, Eq, Generic)

newtype MacroArgument = MacroArgument {unMacroArgument :: HSt.TL.BalancedText}
  deriving stock (Show, Eq, Generic)

newtype MacroArgumentList = MacroArgumentList {unMacroArgumentList :: Seq MacroArgument}
  deriving stock (Show, Eq, Generic)

lookupArg :: ST.ParameterNumber -> MacroArgumentList -> Maybe MacroArgument
lookupArg p argList =
  let argIx = pred (fromEnum p.unParameterNumber)
   in (argList.unMacroArgumentList) Seq.!? argIx

data ExpansionCommand
  = CallMacro ST.MacroDefinition MacroArgumentList
  | ApplyConditionHead ConditionHead
  | ApplyConditionBody ST.ConditionBodyTok
  | RenderNumber HexInt
  | RenderRomanNumeral HexInt
  | RenderTokenAsTokens LT.LexToken
  | RenderJobName
  | RenderFontName FontRef
  | RenderTokenMeaning LT.LexToken
  | ParseControlSequence LT.ControlSequence
  | ExpandAfter LT.LexToken LT.LexToken
  | NoExpand LT.LexToken
  | GetMarkRegister ST.MarkRegister
  | ReadFile HexFilePath
  | EndInputFile
  | RenderInternalQuantity InternalQuantity
  | ChangeCase VDirection HSt.TL.BalancedText
