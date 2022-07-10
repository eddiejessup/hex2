-- Re-export 'LoadedCharSource' type, to allow use of 'get/put' the source,
-- without exposing the innards.
module Hex.Common.HexInput.Interface.CharSource
  (LoadedCharSource)
  where

import Hex.Common.HexInput.Impl.CharSource (LoadedCharSource)
