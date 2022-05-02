module Hex.Run.Evaluate where

import Hexlude
import Hex.Run.App (App)
import Hex.Common.HexState.Impl ()
import Hex.Stage.Categorise.Impl ()
import Hex.Stage.Lex.Impl ()
import Hex.Stage.Resolve.Impl ()
import Hex.Stage.Expand.Impl ()
import Hex.Stage.Parse.Impl ()
import Hex.Stage.Evaluate.Impl ()
import qualified Formatting as F
import Hex.Stage.Evaluate.Interface.AST.Command (Command)
import Hex.Stage.Evaluate.Interface (getEvalCommand)

evaluateAll :: App [Command]
evaluateAll = go
  where
    go =
      getEvalCommand >>= \case
        Nothing ->
          pure []
        Just r -> do
          v <- go
          pure $ r : v

fmtEvalCommandList :: Fmt [Command] r
fmtEvalCommandList = F.unlined fmtOneCommand
  where
    fmtOneCommand =
      F.shown
