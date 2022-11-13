{-# LANGUAGE TemplateHaskell #-}

module Hex.Capability.Log.Interface where

import Data.Text.Lazy.Builder qualified as Tx
import Formatting qualified as F
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

fmtLevel :: Fmt LogLevel
fmtLevel = F.later (Tx.fromText . showLevelEqualWidth)

-- | A source line-number
newtype LineNr = LineNr {unLineNr :: Int}
  deriving stock (Show, Generic)
  deriving newtype (Eq, Enum)

succLineNr :: LineNr -> LineNr
succLineNr = LineNr . succ . unLineNr

fmtLineNr :: Fmt LineNr
fmtLineNr = F.later $ \ln ->
  F.bformat (F.rfixed 4 ' ' F.int) ln.unLineNr

data SrcLoc = SrcLoc {name :: Text, lineNr :: LineNr}
  deriving stock (Show, Generic)

fmtSrcLoc :: Fmt SrcLoc
fmtSrcLoc =
  F.accessed (.name) (F.rfixed 20 ' ' F.stext)
    |%| " "
    <> F.accessed (.lineNr) fmtLineNr

succSrcLoc :: SrcLoc -> SrcLoc
succSrcLoc srcLoc = srcLoc & #lineNr %~ succLineNr

newSrcLoc :: Text -> SrcLoc
newSrcLoc name = SrcLoc {name, lineNr = LineNr 1}

data HexLog :: Effect where
  Log :: LogLevel -> Text -> HexLog m ()
  LogInternalState :: LogLevel -> HexLog m ()

makeEffect ''HexLog

debugLog :: (HexLog :> es) => Text -> Eff es ()
debugLog = log Debug

infoLog :: (HexLog :> es) => Text -> Eff es ()
infoLog = log Info
