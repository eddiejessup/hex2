module Hex.Common.HexState.Interface.Hyphen where

import Hex.Common.Codes qualified as Code
import Hexlude

data HyphenationPattern
  = HyphenationPattern (NonEmpty (Word8, Code.CharCode)) Word8
  deriving stock (Show, Eq, Generic)

-- `Nothing` represents a hyphen.
data HyphenationException
  = HyphenationException (NonEmpty (Maybe Code.CharCode))
  deriving stock (Show, Eq, Generic)
