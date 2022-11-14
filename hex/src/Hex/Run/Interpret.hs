module Hex.Run.Interpret where

import Hex.Capability.Log.Interface (HexLog)
import Hex.Common.HexIO.Interface (HexIO)
import Hex.Common.HexState.Interface (EHexState)
import Hex.Stage.Build.ListElem (HListElem, VListElem)
import Hex.Stage.Build.ListExtractor.Impl (extractMainVListImpl)
import Hex.Stage.Build.ListExtractor.Interface (EndHListReason, ExtractList, IndentFlag (..), extractParagraphList)
import Hex.Stage.Evaluate.Interface (HexEvaluate)
import Hex.Stage.Interpret.AllMode (InterpretError)
import Hex.Stage.Parse.Interface (CommandSource)
import Hexlude

extractMainVList ::
  forall es.
  ( HexEvaluate :> es,
    HexIO :> es,
    CommandSource :> es,
    EHexState :> es,
    Error InterpretError :> es,
    HexLog :> es
  ) =>
  Eff es (Seq VListElem)
extractMainVList = extractMainVListImpl

extractParaHList ::
  ExtractList :> es =>
  Eff es (EndHListReason, Seq HListElem)
extractParaHList = extractParagraphList Indent
