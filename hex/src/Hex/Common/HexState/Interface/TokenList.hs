module Hex.Common.HexState.Interface.TokenList where

import Hexlude
import qualified Hex.Stage.Lex.Interface.Extract as Lex
import qualified Formatting as F

newtype BalancedText = BalancedText {unBalancedText :: Seq Lex.LexToken}
  deriving stock (Show, Generic)
  deriving newtype (Eq, Semigroup, Monoid)

emptyBalancedText :: BalancedText
emptyBalancedText = mempty

newtype InhibitedBalancedText = InhibitedBalancedText {unInhibitedBalancedText :: BalancedText}
  deriving stock (Show, Generic)
  deriving newtype (Eq, Semigroup, Monoid)

newtype ExpandedBalancedText = ExpandedBalancedText {unExpandedBalancedText :: BalancedText}
  deriving stock (Show, Generic)
  deriving newtype (Eq, Semigroup, Monoid)

fmtBalancedText :: Fmt BalancedText
fmtBalancedText = F.shown
