module Hex.Common.HexState.Interface.TokenList where

import Formatting qualified as F
import Hex.Common.Token.Lexed qualified as LT
import Hexlude

newtype BalancedText = BalancedText {unBalancedText :: Seq LT.LexToken}
  deriving stock (Show, Generic)
  deriving newtype (Eq, Semigroup, Monoid)

emptyBalancedText :: BalancedText
emptyBalancedText = mempty

fmtBalancedText :: Fmt BalancedText
fmtBalancedText = F.shown
