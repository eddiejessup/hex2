module Hex.Run.Interpret where

import Hex.Capability.Log.Interface (HexLog)
import Hex.Common.HexState.Interface (EHexState)
import Hex.Stage.Build.ListElem (HList (..), VList (..))
import Hex.Stage.Build.ListExtractor.Interface (EndHListReason, ExtractHList, IndentFlag (..), extractParagraphList)
import Hex.Stage.Build.ListExtractor.VList (extractMainVListImpl)
import Hex.Stage.Evaluate.Interface (HexEvaluate)
import Hex.Stage.Interpret.AllMode (InterpretError)
import Hex.Stage.Parse.Interface (CommandSource)
import Hex.Stage.Read.Interface (HexInput)
import Hexlude

extractMainVList ::
  forall es.
  ( HexEvaluate :> es,
    HexInput :> es,
    CommandSource :> es,
    EHexState :> es,
    Error InterpretError :> es,
    HexLog :> es,
    ExtractHList :> es
  ) =>
  Eff es VList
extractMainVList = extractMainVListImpl

extractParaHList :: ExtractHList :> es => Eff es (EndHListReason, HList)
extractParaHList = extractParagraphList Indent
