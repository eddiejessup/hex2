module Hex.Common.HexState.Interface.TokenList where

import Hexlude
import qualified Hex.Stage.Lex.Interface.Extract as Lex

newtype BalancedText = BalancedText {unBalancedText :: Seq Lex.LexToken}
  deriving stock (Show, Generic)
  deriving newtype (Eq, Semigroup, Monoid)
