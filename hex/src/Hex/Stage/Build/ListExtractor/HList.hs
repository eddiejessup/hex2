{-# LANGUAGE UndecidableInstances #-}
{-# OPTIONS_GHC -Wno-orphans #-}

module Hex.Stage.Build.ListExtractor.HList where

import Hex.Capability.Log.Interface (HexLog (..))
import Hex.Common.HexInput.Interface qualified as HIn
import Hex.Common.HexState.Interface qualified as HSt
import Hex.Stage.Build.ListBuilder.Horizontal (runHListBuilder, runHexListBuilderHMode)
import Hex.Stage.Build.ListBuilder.Interface qualified as Build
import Hex.Stage.Build.ListElem (HList (..))
import Hex.Stage.Build.ListExtractor.Interface qualified as ListExtractor
import Hex.Stage.Evaluate.Interface qualified as Eval
import Hex.Stage.Interpret.AllMode qualified as AllMode
import Hex.Stage.Interpret.HMode qualified as Command.H
import Hex.Stage.Interpret.HMode qualified as HMode
import Hex.Stage.Parse.Interface (CommandSource (..))
import Hexlude

runListExtractor :: [Eval.HexEvaluate, CommandSource, HSt.EHexState, HIn.HexInput, Error AllMode.InterpretError, HexLog] :>> es => Eff (ListExtractor.ExtractHList : es) a -> Eff es a
runListExtractor = interpret $ \_ -> \case
  ListExtractor.ExtractHBoxList -> snd <$> extractHBoxListImpl
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
  Eff es (ListExtractor.EndHListReason, HList)
extractHBoxListImpl =
  runStateLocal mempty $
    runHListBuilder $
      runHexListBuilderHMode $
        buildHListImpl ListExtractor.InnerModeContext

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
        buildHListImpl ListExtractor.OuterModeContext

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
  ListExtractor.ModeContext ->
  Eff es ListExtractor.EndHListReason
buildHListImpl modeCtx = go
  where
    go :: Eff es ListExtractor.EndHListReason
    go = do
      -- Get the state before reading the command,
      -- in case we need to revert to before the command.
      sPreParse <- HIn.getInput
      -- Read the next command.
      command <- AllMode.getNextCommandLogged
      -- Handle the next command, passing the old state in case we need to revert.
      runListExtractor (HMode.handleCommandInHMode sPreParse modeCtx command) >>= \case
        Command.H.EndHList endReason -> pure endReason
        Command.H.ContinueHMode -> go
