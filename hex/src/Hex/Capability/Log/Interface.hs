module Hex.Capability.Log.Interface where

import Hexlude

data LogLevel = Debug | Info | Warn | Error | Fatal
  deriving stock (Eq, Ord, Show, Generic)

readLogLevel :: (IsString s, Eq s) => s -> Maybe LogLevel
readLogLevel = \case
  "debug" -> Just Debug
  "info" -> Just Info
  "warn" -> Just Warn
  "error" -> Just Error
  "fatal" -> Just Fatal
  _ -> Nothing

showLevelEqualWidth :: LogLevel -> Text
showLevelEqualWidth = \case
  Debug -> "DEBUG"
  Info -> "INFO "
  Warn -> "WARN "
  Error -> "ERROR"
  Fatal -> "FATAL"

class Monad m => MonadHexLog m where
  log :: LogLevel -> Text -> m ()

  logInternalState :: m ()

debugLog :: MonadHexLog m => Text -> m ()
debugLog = log Debug

infoLog :: MonadHexLog m => Text -> m ()
infoLog = log Info

instance MonadHexLog m => MonadHexLog (StateT s m) where
  log x y = lift $ log x y

  logInternalState = lift logInternalState
