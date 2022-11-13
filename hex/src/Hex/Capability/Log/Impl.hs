module Hex.Capability.Log.Impl where

import Formatting qualified as F
import Hex.Capability.Log.Interface
import Hex.Common.HexEnv.Impl (HexEnv)
import Hex.Common.HexEnv.Impl qualified as HEnv
import Hex.Common.HexIO.Impl.CharSource qualified as CharSource
import Hex.Common.HexIO.Impl.IOState (IOState)
import Hex.Common.HexIO.Impl.IOState qualified as IOState
import Hex.Common.HexState.Impl.Type qualified as HSt
import Hexlude

runLog ::
  (Reader HexEnv :> es, State HSt.HexState :> es, State IOState :> es, IOE :> es) =>
  Eff (HexLog : es) a ->
  Eff es a
runLog = interpret $ \_ -> \case
  Log lvl msg -> logImpl lvl msg
  LogInternalState lvl -> logInternalStateImpl lvl

logInternalStateImpl ::
  (Reader HexEnv :> es, State HSt.HexState :> es, State IOState :> es, IOE :> es) =>
  LogLevel ->
  Eff es ()
logInternalStateImpl lvl = do
  st <- get @HSt.HexState
  logImpl lvl (sformat HSt.fmtHexState st)

logImpl ::
  (Reader HexEnv :> es, State IOState :> es, IOE :> es) =>
  LogLevel ->
  Text ->
  Eff es ()
logImpl lvl msg = do
  logFileHandle <- know @HexEnv #logHandle
  logLevel <- know @HexEnv $ #logLevel
  srcLoc <- use @IOState (IOState.ioStateCurrentSourceLens % #srcLoc)
  when (lvl >= logLevel) $
    hPutStrLn logFileHandle $
      F.sformat (fmtLevel |%| " " |%| fmtSrcLoc |%| " " |%| F.stext) lvl srcLoc msg
