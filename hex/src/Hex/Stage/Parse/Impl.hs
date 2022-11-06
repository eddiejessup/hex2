{-# LANGUAGE UndecidableInstances #-}

module Hex.Stage.Parse.Impl where

import Hex.Capability.Log.Interface qualified as Log
import Hex.Common.HexIO.Interface qualified as HIO
import Hex.Common.HexState.Interface qualified as HSt
import Hex.Stage.Evaluate.Impl.Common qualified as Eval
import Hex.Stage.Expand.Impl (runAltPrimTokenSourceMaybe)
import Hex.Stage.Expand.Interface (ParsingError)
import Hex.Stage.Expand.Interface qualified as Exp
import Hex.Stage.Parse.Impl.Parsers.Command qualified as Parsers.Command
import Hex.Stage.Parse.Interface
import Hex.Stage.Parse.Interface.AST.Command qualified as Par
import Hexlude

runCommandSource ::
  ( Error Exp.ExpansionError :> es,
    Error ParsingError :> es,
    Error Eval.EvaluationError :> es,
    Error HSt.ResolutionError :> es,
    HIO.HexIO :> es,
    HSt.EHexState :> es,
    State Exp.ConditionStates :> es,
    Log.HexLog :> es
  ) =>
  Eff (CommandSource : es) a ->
  Eff es a
runCommandSource = interpret $ \_ -> \case
  GetCommand -> getCommandImpl

getCommandImpl ::
  ( Error Exp.ExpansionError :> es,
    Error ParsingError :> es,
    Error Eval.EvaluationError :> es,
    Error HSt.ResolutionError :> es,
    HIO.HexIO :> es,
    HSt.EHexState :> es,
    State Exp.ConditionStates :> es,
    Log.HexLog :> es
  ) =>
  Eff es (Maybe Par.Command)
getCommandImpl = do
  runAltPrimTokenSourceMaybe Parsers.Command.parseCommand >>= \case
    Nothing -> pure Nothing
    Just cmd -> do
      -- Log.infoLog $ F.sformat ("Parsed command: " |%| fmtParseLog) pLog
      pure $ Just cmd
