module Hex.Stage.Parse.Interface.AST.Condition where

import Hex.Stage.Parse.Interface.AST.Quantity
import Hex.Common.HexState.Interface.Resolve.PrimitiveToken qualified as PT
import Hex.Common.HexState.Interface.Resolve.SyntaxToken qualified as PT.Syn
import Hexlude
import qualified Hex.Stage.Lex.Interface.Extract as Lex

data IfConditionHead
  = IfIntPairTest HexInt Ordering HexInt -- \ifnum
  | IfLengthPairTest Length Ordering Length -- \ifdim
  | IfIntOdd HexInt -- \ifodd
  | IfInMode PT.ModeAttribute -- \ifvmode, \ifhmode, \ifmmode, \ifinner
  | IfTokenAttributesEqual PT.Syn.TokenAttribute PT.PrimitiveToken PT.PrimitiveToken -- \if, \ifcat
  | IfTokensEqual Lex.LexToken Lex.LexToken -- \ifx
  | IfBoxRegisterIs PT.BoxRegisterAttribute HexInt -- \ifvoid, \ifhbox, \ifvbox
  | IfInputEnded HexInt -- \ifeof
  | IfConst Bool -- \iftrue, \iffalse
  deriving stock (Show, Eq, Generic)

data ConditionHead = IfConditionHead IfConditionHead | CaseConditionHead HexInt
  deriving stock (Show, Eq, Generic)
