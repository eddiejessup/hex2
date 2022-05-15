module Hex.Stage.Parse.Interface.AST.SyntaxCommand where

import Data.Sequence qualified as Seq
import Hex.Common.HexState.Interface.Resolve.PrimitiveToken qualified as PT
import Hex.Common.HexState.Interface.Resolve.SyntaxToken qualified as PT.Syn
import Hex.Common.HexState.Interface.Resolve.SyntaxToken qualified as ST
import Hex.Common.Quantity qualified as Q
import Hex.Stage.Lex.Interface.Extract qualified as Lex
import Hex.Stage.Parse.Interface.AST.Command (InternalQuantity)
import Hex.Stage.Parse.Interface.AST.Quantity
import Hexlude

data IfConditionHead
  = IfIntPairTest HexInt Ordering HexInt -- \ifnum
  | IfLengthPairTest Length Ordering Length -- \ifdim
  | IfIntOdd HexInt -- \ifodd
  | IfInMode PT.ModeAttribute -- \ifvmode, \ifhmode, \ifmmode, \ifinner
  | IfTokenAttributesEqual PT.Syn.TokenAttribute Lex.LexToken Lex.LexToken -- \if, \ifcat
  | IfTokensEqual Lex.LexToken Lex.LexToken -- \ifx
  | IfBoxRegisterIs PT.BoxRegisterAttribute HexInt -- \ifvoid, \ifhbox, \ifvbox
  | IfInputEnded HexInt -- \ifeof
  | IfConst Bool -- \iftrue, \iffalse
  deriving stock (Show, Eq, Generic)

data ConditionHead = IfConditionHead IfConditionHead | CaseConditionHead HexInt
  deriving stock (Show, Eq, Generic)

newtype MacroArgument = MacroArgument {unMacroArgument :: ST.InhibitedBalancedText}
  deriving stock (Show, Eq, Generic)

newtype MacroArgumentList = MacroArgumentList {unMacroArgumentList :: Seq MacroArgument}
  deriving stock (Show, Eq, Generic)

lookupArg :: ST.ParameterNumber -> MacroArgumentList -> Maybe MacroArgument
lookupArg p argList =
  let argIx = fromEnum p.unParameterNumber - 1
   in (argList.unMacroArgumentList) Seq.!? argIx

data SyntaxCommand
  = CallMacro ST.MacroDefinition MacroArgumentList
  | ApplyConditionHead ConditionHead
  | ApplyConditionBody ST.ConditionBodyTok
  | RenderNumber HexInt
  | RenderRomanNumeral HexInt
  | RenderTokenAsTokens Lex.LexToken
  | RenderJobName
  | RenderFontName FontRef
  | RenderTokenMeaning Lex.LexToken
  | ParseControlSequence ByteString
  | ExpandAfter Lex.LexToken
  | NoExpand Lex.LexToken
  | GetMarkRegister ST.MarkRegister
  | OpenInputFile Q.HexFilePath
  | EndInputFile
  | RenderInternalQuantity InternalQuantity
  | ChangeCase Q.VDirection ST.InhibitedBalancedText
