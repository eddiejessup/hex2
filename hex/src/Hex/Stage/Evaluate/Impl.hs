module Hex.Stage.Evaluate.Impl where

import Hex.Common.HexState.Interface (EHexState)
import Hex.Stage.Evaluate.Impl.Command qualified as Eval
import Hex.Stage.Evaluate.Impl.Common qualified as Eval
import Hex.Stage.Evaluate.Interface (HexEvaluate (..))
import Hexlude

runHexEvaluate :: ((Error Eval.EvaluationError :> es, EHexState :> es)) => Eff (HexEvaluate : es) a -> Eff es a
runHexEvaluate = interpret $ \_ -> \case
  EvalCommand cmd -> Eval.evalCommand cmd
