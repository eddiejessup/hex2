{-# LANGUAGE UndecidableInstances #-}
{-# OPTIONS_GHC -Wno-orphans #-}

module Hex.Stage.Build.ListExtractor.Impl where

import Hex.Capability.Log.Interface (HexLog (..))
import Hex.Common.HexState.Interface qualified as HSt
import Hex.Common.HexState.Interface.Mode qualified as HSt.Mode
import Hex.Stage.Build.ListBuilder.Horizontal (runHListBuilder, runHexListBuilderHMode)
import Hex.Stage.Build.ListBuilder.Interface qualified as Build
import Hex.Stage.Build.ListBuilder.Vertical (runHexListBuilderVMode)
import Hex.Stage.Build.ListBuilder.Vertical qualified as Build.V
import Hex.Stage.Build.ListElem (HList (..))
import Hex.Stage.Build.ListElem qualified as ListElem
import Hex.Stage.Build.ListExtractor.Interface qualified as ListExtractor
import Hex.Stage.Evaluate.Interface qualified as Eval
import Hex.Stage.Interpret.AllMode qualified as AllMode
import Hex.Stage.Interpret.HMode qualified as Command.H
import Hex.Stage.Interpret.VMode qualified as Command.V
import Hex.Stage.Parse.Interface (CommandSource (..))
import Hex.Stage.Read.Interface qualified as HIn
import Hexlude

runListExtractor :: [Eval.HexEvaluate, CommandSource, HSt.EHexState, HIn.HexInput, Error AllMode.InterpretError, HexLog] :>> es => Eff (ListExtractor.ExtractList : es) a -> Eff es a
runListExtractor = interpret $ \_ -> \case
  ListExtractor.ExtractHBoxList -> extractHBoxListImpl
  ListExtractor.ExtractVBoxList -> extractVBoxListImpl
  ListExtractor.ExtractParagraphList indentFlag -> extractParagraphListImpl indentFlag

extractHBoxListImpl ::
  forall es.
  ( Eval.HexEvaluate :> es,
    CommandSource :> es,
    HSt.EHexState :> es,
    HIn.HexInput :> es,
    Error AllMode.InterpretError :> es,
    HexLog :> es
  ) =>
  Eff es HList
extractHBoxListImpl =
  execStateLocal mempty $
    runHListBuilder $
      runHexListBuilderHMode $
        buildHListImpl HSt.Mode.InnerModeVariant

extractParagraphListImpl ::
  forall es.
  ( Eval.HexEvaluate :> es,
    CommandSource :> es,
    HSt.EHexState :> es,
    HIn.HexInput :> es,
    Error AllMode.InterpretError :> es,
    HexLog :> es
  ) =>
  ListExtractor.IndentFlag ->
  Eff es (ListExtractor.EndHListReason, HList)
extractParagraphListImpl indentFlag = do
  initList <-
    HList <$> case indentFlag of
      ListExtractor.Indent ->
        singleton <$> HSt.getParIndentBox
      ListExtractor.DoNotIndent ->
        pure mempty
  runStateLocal initList $
    runHListBuilder $
      runHexListBuilderHMode $
        buildHListImpl HSt.Mode.OuterModeVariant

buildHListImpl ::
  forall es.
  ( Eval.HexEvaluate :> es,
    CommandSource :> es,
    HSt.EHexState :> es,
    HIn.HexInput :> es,
    Build.HListBuilder :> es,
    Build.HexListBuilder :> es,
    Error AllMode.InterpretError :> es,
    HexLog :> es
  ) =>
  HSt.Mode.ModeVariant ->
  Eff es ListExtractor.EndHListReason
buildHListImpl modeVariant = do
  HSt.enterMode $ HSt.Mode.hModeFromVariant modeVariant
  go
  where
    go :: Eff es ListExtractor.EndHListReason
    go = do
      -- Get the state before reading the command,
      -- in case we need to revert to before the command.
      sPreParse <- HIn.getInput
      -- Read the next command.
      command <- AllMode.getNextCommandLogged
      -- Handle the next command, passing the old state in case we need to revert.
      runListExtractor (Command.H.handleCommandInHMode sPreParse modeVariant command) >>= \case
        Command.H.EndHList endReason -> do
          HSt.leaveMode
          pure endReason
        Command.H.ContinueHMode ->
          go

extractMainVListImpl ::
  forall es.
  ( Eval.HexEvaluate :> es,
    HIn.HexInput :> es,
    CommandSource :> es,
    HSt.EHexState :> es,
    Error AllMode.InterpretError :> es,
    HexLog :> es
  ) =>
  Eff es ListElem.VList
extractMainVListImpl =
  execStateLocal (ListElem.VList Empty) $
    Build.V.runHexListBuilderVMode $
      buildVListImpl HSt.Mode.OuterModeVariant

extractVBoxListImpl ::
  forall es.
  ( Eval.HexEvaluate :> es,
    CommandSource :> es,
    HSt.EHexState :> es,
    HIn.HexInput :> es,
    Error AllMode.InterpretError :> es,
    HexLog :> es
  ) =>
  Eff es ListElem.VList
extractVBoxListImpl =
  execStateLocal mempty $
    runHexListBuilderVMode $
      buildVListImpl HSt.Mode.InnerModeVariant

buildVListImpl ::
  forall es.
  ( Eval.HexEvaluate :> es,
    HIn.HexInput :> es,
    CommandSource :> es,
    HSt.EHexState :> es,
    Build.HexListBuilder :> es,
    Error AllMode.InterpretError :> es,
    HexLog :> es
  ) =>
  HSt.Mode.ModeVariant ->
  Eff es ()
buildVListImpl modeVariant = do
  case HSt.Mode.vModeFromVariant modeVariant of
    Nothing -> pure ()
    Just mode -> HSt.enterMode mode
  go
  where
    go :: Eff es ()
    go = do
      streamPreParse <- HIn.getInput
      command <- AllMode.getNextCommandLogged
      runListExtractor (Command.V.handleCommandInVMode streamPreParse modeVariant command) >>= \case
        Command.V.EndMainVMode ->
          case modeVariant of
            HSt.Mode.InnerModeVariant ->
              HSt.leaveMode
            HSt.Mode.OuterModeVariant ->
              pure ()
        Command.V.ContinueMainVMode ->
          go
