{-# LANGUAGE UndecidableInstances #-}
{-# OPTIONS_GHC -Wno-orphans #-}

module Hex.Stage.Build.ListExtractor.Impl where

import Hex.Capability.Log.Interface (HexLog (..))
import Hex.Common.HexIO.Interface qualified as HIO
import Hex.Common.HexState.Interface qualified as HSt
import Hex.Common.HexState.Interface.Mode qualified as HSt.Mode
import Hex.Common.HexState.Interface.Parameter qualified as HSt.Param
import Hex.Common.Quantity qualified as Q
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
import Hexlude

runListExtractor :: (Eval.HexEvaluate :> es, CommandSource :> es, HSt.EHexState :> es, HIO.HexIO :> es, Error AllMode.InterpretError :> es, HexLog :> es) => Eff (ListExtractor.ExtractList : es) a -> Eff es a
runListExtractor = interpret $ \_ -> \case
  ListExtractor.ExtractHBoxList -> extractHBoxListImpl
  ListExtractor.ExtractVBoxList -> extractVBoxListImpl
  ListExtractor.ExtractParagraphList indentFlag -> extractParagraphListImpl indentFlag

extractHBoxListImpl ::
  forall es.
  ( Eval.HexEvaluate :> es,
    CommandSource :> es,
    HSt.EHexState :> es,
    HIO.HexIO :> es,
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
    HIO.HexIO :> es,
    Error AllMode.InterpretError :> es,
    HexLog :> es
  ) =>
  ListExtractor.IndentFlag ->
  Eff es (ListExtractor.EndHListReason, HList)
extractParagraphListImpl indentFlag = do
  initList <-
    case indentFlag of
      ListExtractor.Indent -> do
        parDims <- HSt.getParIndentBoxDims
        pure $ singleton $ HSt.emptyHBoxAsHListElem parDims
      ListExtractor.DoNotIndent ->
        pure mempty
  runStateLocal (HList initList) $
    runHListBuilder $
      runHexListBuilderHMode $
        buildHListImpl HSt.Mode.OuterModeVariant

buildHListImpl ::
  forall es.
  ( Eval.HexEvaluate :> es,
    CommandSource :> es,
    HSt.EHexState :> es,
    HIO.HexIO :> es,
    Build.HListBuilder :> es,
    Build.HexListBuilder :> es,
    Error AllMode.InterpretError :> es,
    HexLog :> es
  ) =>
  HSt.Mode.ModeVariant ->
  Eff es ListExtractor.EndHListReason
buildHListImpl modeVariant = do
  HSt.enterMode $ HSt.Mode.hModeFromVariant modeVariant
  -- The space factor f is 1000 at the beginning of a horizontal list
  HSt.setSpecialIntParameter HSt.Param.SpaceFactor Q.thousandInt
  go
  where
    go :: Eff es ListExtractor.EndHListReason
    go = do
      -- Get the state before reading the command,
      -- in case we need to revert to before the command.
      sPreParse <- HIO.getInput
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
    HIO.HexIO :> es,
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
    HIO.HexIO :> es,
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
    HIO.HexIO :> es,
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
      streamPreParse <- HIO.getInput
      command <- AllMode.getNextCommandLogged
      runListExtractor (Command.V.handleCommandInVMode streamPreParse modeVariant command) >>= \case
        Command.V.EndVMode ->
          case modeVariant of
            HSt.Mode.InnerModeVariant ->
              HSt.leaveMode
            HSt.Mode.OuterModeVariant ->
              pure ()
        Command.V.ContinueVMode ->
          go
