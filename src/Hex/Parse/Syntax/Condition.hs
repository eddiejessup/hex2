module Hex.Parse.Syntax.Condition where

import Hex.Lex.Types qualified as H.Lex
import Hex.Parse.Syntax.Quantity
import Hex.Symbol.Token.Primitive qualified as H.Sym.Tok
import Hex.Symbol.Token.SyntaxCommandHead qualified as H.Sym.Tok.Syn
import Hexlude

data IfConditionHead
  = IfIntPairTest HexInt Ordering HexInt -- \ifnum
  | IfLengthPairTest Length Ordering Length -- \ifdim
  | IfIntOdd HexInt -- \ifodd
  | IfInMode H.Sym.Tok.ModeAttribute -- \ifvmode, \ifhmode, \ifmmode, \ifinner
  | IfTokenAttributesEqual H.Sym.Tok.Syn.TokenAttribute H.Sym.Tok.PrimitiveToken H.Sym.Tok.PrimitiveToken -- \if, \ifcat
  | IfTokensEqual H.Lex.LexToken H.Lex.LexToken -- \ifx
  | IfBoxRegisterIs H.Sym.Tok.BoxRegisterAttribute HexInt -- \ifvoid, \ifhbox, \ifvbox
  | IfInputEnded HexInt -- \ifeof
  | IfConst Bool -- \iftrue, \iffalse
  deriving stock (Generic)

data ConditionHead = IfConditionHead IfConditionHead | CaseConditionHead HexInt
  deriving stock (Generic)
