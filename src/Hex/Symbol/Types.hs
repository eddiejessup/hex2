module Hex.Symbol.Types where


import Hexlude
import qualified Hex.Codes as H.Codes
import qualified Hex.Lex.Types as H.Lex

data ControlSymbol
  = ActiveCharacterSymbol H.Codes.CharCode
  | ControlSequenceSymbol H.Lex.ControlSequence
  deriving stock (Show, Eq, Generic)
  deriving anyclass Hashable
