{-# LANGUAGE UndecidableInstances #-}

module Hex.Stage.Parse.Impl where

import Hex.Capability.Log.Interface qualified as Log
import Hex.Common.HexInput.Interface qualified as HIn
import Hex.Common.Parse.Impl (runAltPrimTokenParseMaybe)
import Hex.Common.Parse.Interface (ParseUnexpectedError)
import Hex.Stage.Expand.Interface qualified as Exp
import Hex.Stage.Parse.Impl.Parsers.Command qualified as Parsers.Command
import Hex.Stage.Parse.Interface
import Hex.Stage.Parse.Interface.AST.Command qualified as Par
import Hexlude

runCommandSource ::
  [HIn.HexInput, Exp.PrimTokenSource, Error ParseUnexpectedError, Log.HexLog] :>> es =>
  Eff (CommandSource : es) a ->
  Eff es a
runCommandSource = interpret $ \_ -> \case
  GetCommand -> getCommandImpl

getCommandImpl ::
  [HIn.HexInput, Exp.PrimTokenSource, Error ParseUnexpectedError, Log.HexLog] :>> es =>
  Eff es (Maybe Par.Command)
getCommandImpl = do
  mayCmd <- runAltPrimTokenParseMaybe Parsers.Command.parseCommand
  case mayCmd of
    Nothing -> pure Nothing
    Just cmd -> do
      -- Log.infoLog $ F.sformat ("Parsed command: " |%| fmtParseLog) pLog
      pure $ Just cmd
