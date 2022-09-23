-- Re-export 'CharSource' type, to allow use of 'get/put' the source,
-- without exposing the innards.
module Hex.Common.HexIO.Interface.CharSourceStack (CharSourceStack) where

import Hex.Common.HexIO.Impl.CharSourceStack (CharSourceStack)
