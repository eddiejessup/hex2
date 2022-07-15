{-# LANGUAGE UndecidableInstances #-}
{-# OPTIONS_GHC -Wno-orphans #-}

module Hex.Stage.Build.ListExtractor.VList where

import Hex.Capability.Log.Interface (MonadHexLog (..))
import Hex.Common.HexInput.Interface qualified as HIn
import Hex.Common.HexState.Interface qualified as HSt
import Hex.Stage.Build.ListBuilder.Vertical qualified as Build.V
import Hex.Stage.Build.ListElem qualified as H.Inter.B.List
import Hex.Stage.Build.ListExtractor.HList ()
import Hex.Stage.Evaluate.Interface qualified as Eval
import Hex.Stage.Interpret.CommandHandler.AllMode qualified as AllMode
import Hex.Stage.Interpret.CommandHandler.VMode qualified as Command.V
import Hex.Stage.Parse.Interface (MonadCommandSource (..))
import Hexlude

extractMainVListImpl ::
  forall e m.
  ( Eval.MonadEvaluate m,
    HIn.MonadHexInput m,
    MonadCommandSource m,
    HSt.MonadHexState m,
    MonadError e m,
    AsType AllMode.InterpretError e,
    MonadHexLog m
  ) =>
  m H.Inter.B.List.VList
extractMainVListImpl = Build.V.execVListBuilderT (H.Inter.B.List.VList Empty) go
  where
    go :: Build.V.VListBuilderT m ()
    go = do
      streamPreParse <- HIn.getInput
      command <- AllMode.getNextCommandLogged
      Command.V.handleCommandInMainVMode streamPreParse command >>= \case
        Command.V.EndMainVMode -> pure ()
        Command.V.ContinueMainVMode -> go
