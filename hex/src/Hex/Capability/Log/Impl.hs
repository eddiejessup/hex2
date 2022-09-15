module Hex.Capability.Log.Impl where

import Hex.Capability.Log.Interface
import Hex.Common.HexEnv.Impl (HexEnv)
import Hex.Common.HexState.Impl.Type (HexState)
import Hex.Common.HexState.Impl.Type qualified as HSt
import Hexlude

runLog :: [IOE, Reader HexEnv, State HexState] :>> es => Eff (HexLog : es) a -> Eff es a
runLog = interpret $ \_ -> \case
  Log lvl msg -> logImpl lvl msg
  LogInternalState -> do
    hexState <- get @HexState
    logImpl Info $ sformat HSt.fmtHexState hexState

logImpl :: [IOE, Reader HexEnv, State HexState] :>> es => LogLevel -> Text -> Eff es ()
logImpl lvl msg = do
  logFileHandle <- know @HexEnv $ typed @Handle
  logLevel <- know @HexEnv $ typed @LogLevel
  when (lvl >= logLevel) $
    liftIO $
      hPutStrLn logFileHandle $
        showLevelEqualWidth lvl <> " " <> msg
