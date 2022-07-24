-- Re-export 'CharSource' type, to allow use of 'get/put' the source,
-- without exposing the innards.
module Hex.Stage.Read.Interface.CharSourceStack (CharSourceStack) where

import Hex.Stage.Read.Impl.CharSourceStack (CharSourceStack)
