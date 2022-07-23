module Hex.Run.Evaluate where

import Formatting qualified as F
import Hex.Stage.Evaluate.Interface (HexEvaluate, getEvalCommand)
import Hex.Stage.Evaluate.Interface.AST.Command (Command)
import Hex.Stage.Parse.Interface (CommandSource)
import Hexlude

evaluateAll :: [CommandSource, HexEvaluate] :>> es => Eff es [Command]
evaluateAll = go
  where
    go =
      getEvalCommand >>= \case
        Nothing ->
          pure []
        Just r -> do
          v <- go
          pure $ r : v

fmtEvalCommandList :: Fmt [Command]
fmtEvalCommandList = F.unlined fmtOneCommand
  where
    fmtOneCommand =
      F.shown
