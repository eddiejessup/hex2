module Hex.Stage.Parse.Interface.AST.Condition where

import Hex.Stage.Parse.Interface.AST.Common
import Hex.Common.HexState.Interface.Resolve.PrimitiveToken qualified as H.Sym.Tok
import Hex.Common.HexState.Interface.Resolve.SyntaxToken qualified as H.Sym.Tok.Syn
import Hexlude
import qualified Hex.Stage.Lex.Interface.Extract as Lex

data IfConditionHead
  = IfIntPairTest HexInt Ordering HexInt -- \ifnum
  | IfLengthPairTest Length Ordering Length -- \ifdim
  | IfIntOdd HexInt -- \ifodd
  | IfInMode H.Sym.Tok.ModeAttribute -- \ifvmode, \ifhmode, \ifmmode, \ifinner
  | IfTokenAttributesEqual H.Sym.Tok.Syn.TokenAttribute H.Sym.Tok.PrimitiveToken H.Sym.Tok.PrimitiveToken -- \if, \ifcat
  | IfTokensEqual Lex.LexToken Lex.LexToken -- \ifx
  | IfBoxRegisterIs H.Sym.Tok.BoxRegisterAttribute HexInt -- \ifvoid, \ifhbox, \ifvbox
  | IfInputEnded HexInt -- \ifeof
  | IfConst Bool -- \iftrue, \iffalse
  deriving stock (Show, Eq, Generic)

data ConditionHead = IfConditionHead IfConditionHead | CaseConditionHead HexInt
  deriving stock (Show, Eq, Generic)
