module Hex.Capability.Log.Impl where

import Effectful.FileSystem qualified as FS
import Effectful.Internal.Monad qualified as Eff
import Hex.Capability.Log.Interface
import Hex.Common.HexEnv.Impl (HexEnv)
import Hex.Common.HexEnv.Impl qualified as HEnv
import Hex.Common.HexState.Impl.Type (HexState)
import Hex.Common.HexState.Impl.Type qualified as HSt
import Hexlude

runLog :: [Reader HexEnv, State HexState, FS.FileSystem] :>> es => Eff (HexLog : es) a -> Eff es a
runLog = interpret $ \_ -> \case
  Log lvl msg -> logImpl lvl msg
  LogInternalState -> do
    hexState <- get @HexState
    logImpl Info $ sformat HSt.fmtHexState hexState

logImpl :: [Reader HexEnv, State HexState, FS.FileSystem] :>> es => LogLevel -> Text -> Eff es ()
logImpl lvl msg = do
  logFileHandle <- know @HexEnv #logHandle
  logLevel <- know @HexEnv $ #logLevel
  when (lvl >= logLevel) $
    Eff.unsafeEff_ $
      hPutStrLn logFileHandle $
        showLevelEqualWidth lvl <> " " <> msg
