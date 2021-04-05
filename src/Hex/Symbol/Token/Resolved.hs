module Hex.Symbol.Token.Resolved where

import Hex.Symbol.Token.Primitive
import Hex.Symbol.Token.SyntaxCommandHead
import Hexlude

data ResolvedToken
  = SyntaxCommandHeadToken SyntaxCommandHeadToken
  | PrimitiveToken PrimitiveToken
  deriving stock (Show, Eq, Generic)
