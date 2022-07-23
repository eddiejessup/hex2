{-# LANGUAGE TemplateHaskell #-}
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

data HexLog :: Effect where
  Log :: LogLevel -> Text -> HexLog m ()
  LogInternalState :: HexLog m ()

makeEffect ''HexLog

debugLog :: (HexLog :> es) => Text -> Eff es ()
debugLog = log Debug

infoLog :: (HexLog :> es) => Text -> Eff es ()
infoLog = log Info
