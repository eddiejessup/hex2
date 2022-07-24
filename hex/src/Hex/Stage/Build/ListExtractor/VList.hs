{-# LANGUAGE UndecidableInstances #-}
{-# OPTIONS_GHC -Wno-orphans #-}

module Hex.Stage.Build.ListExtractor.VList where

import Hex.Capability.Log.Interface (HexLog (..))
import Hex.Common.HexState.Interface qualified as HSt
import Hex.Stage.Build.ListBuilder.Interface (HexListBuilder)
import Hex.Stage.Build.ListBuilder.Vertical qualified as Build.V
import Hex.Stage.Build.ListElem qualified as ListElem
import Hex.Stage.Build.ListExtractor.HList ()
import Hex.Stage.Build.ListExtractor.Interface (ExtractHList)
import Hex.Stage.Evaluate.Interface qualified as Eval
import Hex.Stage.Interpret.AllMode qualified as AllMode
import Hex.Stage.Interpret.VMode qualified as Command.V
import Hex.Stage.Parse.Interface (CommandSource (..))
import Hex.Stage.Read.Interface qualified as HIn
import Hexlude

extractMainVListImpl ::
  forall es.
  ( Eval.HexEvaluate :> es,
    HIn.HexInput :> es,
    CommandSource :> es,
    HSt.EHexState :> es,
    Error AllMode.InterpretError :> es,
    HexLog :> es,
    ExtractHList :> es
  ) =>
  Eff es ListElem.VList
extractMainVListImpl = Build.V.runHexListBuilderVMode (ListElem.VList Empty) go
  where
    go :: Eff (HexListBuilder : es) ()
    go = do
      streamPreParse <- HIn.getInput
      command <- AllMode.getNextCommandLogged
      Command.V.handleCommandInMainVMode streamPreParse command >>= \case
        Command.V.EndMainVMode -> pure ()
        Command.V.ContinueMainVMode -> go
