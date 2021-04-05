module Hex.Symbol.Types where

import Hex.Codes qualified as H.Codes
import Hex.Lex.Types qualified as H.Lex
import Hexlude

data ControlSymbol
  = ActiveCharacterSymbol H.Codes.CharCode
  | ControlSequenceSymbol H.Lex.ControlSequence
  deriving stock (Show, Eq, Generic)
  deriving anyclass (Hashable)
