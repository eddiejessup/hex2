{-# LANGUAGE UndecidableInstances #-}
{-# OPTIONS_GHC -Wno-orphans #-}

module Hex.Stage.Build.ListExtractor.HList where

import Hex.Capability.Log.Interface (MonadHexLog (..))
import Hex.Common.HexInput.Interface qualified as HIn
import Hex.Common.HexState.Interface (MonadHexState)
import Hex.Common.HexState.Interface qualified as HSt
import Hex.Stage.Build.ListBuilder.Horizontal qualified as Build.H
import Hex.Stage.Build.ListElem qualified as H.Inter.B.List
import Hex.Stage.Build.ListExtractor.Interface (MonadHexListExtractor)
import Hex.Stage.Build.ListExtractor.Interface qualified as ListExtractor
import Hex.Stage.Evaluate.Interface (MonadEvaluate (..))
import Hex.Stage.Evaluate.Interface qualified as Eval
import Hex.Stage.Interpret.CommandHandler.AllMode (InterpretError)
import Hex.Stage.Interpret.CommandHandler.AllMode qualified as AllMode
import Hex.Stage.Interpret.CommandHandler.HMode qualified as Command.H
import Hex.Stage.Interpret.CommandHandler.HMode qualified as HMode
import Hex.Stage.Parse.Interface (MonadCommandSource (..))
import Hexlude

instance
  ( Monad m,
    MonadEvaluate m,
    MonadCommandSource m,
    MonadHexState m,
    HIn.MonadHexInput m,
    MonadError e m,
    AsType InterpretError e,
    MonadHexLog m
  ) =>
  MonadHexListExtractor m
  where
  extractHBoxList = extractHBoxListImpl

  extractParagraphList = extractParagraphListImpl

extractHBoxListImpl ::
  forall m e.
  ( Eval.MonadEvaluate m,
    MonadCommandSource m,
    HSt.MonadHexState m,
    HIn.MonadHexInput m,
    MonadError e m,
    AsType AllMode.InterpretError e,
    MonadHexLog m
  ) =>
  m H.Inter.B.List.HList
extractHBoxListImpl =
  snd <$> buildHList ListExtractor.InnerModeContext (H.Inter.B.List.HList mempty)

extractParagraphListImpl ::
  forall m e.
  ( Eval.MonadEvaluate m,
    MonadCommandSource m,
    HSt.MonadHexState m,
    HIn.MonadHexInput m,
    MonadError e m,
    AsType AllMode.InterpretError e,
    MonadHexLog m
  ) =>
  ListExtractor.IndentFlag ->
  m (ListExtractor.EndHListReason, H.Inter.B.List.HList)
extractParagraphListImpl indentFlag = do
  initList <- case indentFlag of
    ListExtractor.Indent ->
      singleton <$> HSt.getParIndentBox
    ListExtractor.DoNotIndent ->
      pure mempty
  buildHList ListExtractor.OuterModeContext (H.Inter.B.List.HList initList)

buildHList ::
  forall m e.
  ( Eval.MonadEvaluate m,
    MonadCommandSource m,
    HSt.MonadHexState m,
    HIn.MonadHexInput m,
    MonadError e m,
    AsType AllMode.InterpretError e,
    MonadHexLog m
  ) =>
  ListExtractor.ModeContext ->
  H.Inter.B.List.HList ->
  m (ListExtractor.EndHListReason, H.Inter.B.List.HList)
buildHList modeCtx initList = do
  Build.H.runHListBuilderT initList go
  where
    go :: Build.H.HListBuilderT m ListExtractor.EndHListReason
    go = do
      -- Get the state before reading the command,
      -- in case we need to revert to before the command.
      sPreParse <- HIn.getInput
      -- Read the next command.
      command <- AllMode.getNextCommandLogged
      -- Handle the next command, passing the old state in case we need to revert.
      HMode.handleCommandInHMode sPreParse modeCtx command >>= \case
        Command.H.EndHList endReason -> pure endReason
        Command.H.ContinueHMode -> go
