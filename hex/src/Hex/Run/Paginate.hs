module Hex.Run.Paginate where

import Formatting qualified as F
import Hex.Capability.Log.Interface qualified as Log
import Hex.Common.HexIO.Interface (HexIO)
import Hex.Common.HexState.Interface (EHexState)
import Hex.Common.HexState.Interface qualified as HSt
import Hex.Common.HexState.Interface.Parameter qualified as HSt.Param
import Hex.Common.Quantity qualified as Q
import Hex.Stage.Build.ListElem qualified as ListElem
import Hex.Stage.Build.ListExtractor.Impl (extractMainVListImpl)
import Hex.Stage.Build.Vertical.Page.Break qualified as Page
import Hex.Stage.Build.Vertical.Page.Types qualified as Page
import Hex.Stage.Evaluate.Interface (HexEvaluate)
import Hex.Stage.Interpret.AllMode (InterpretError)
import Hex.Stage.Parse.Interface (CommandSource)
import Hexlude

paginateAll :: (Error InterpretError :> es, HexEvaluate :> es, HexIO :> es, CommandSource :> es, EHexState :> es, Log.HexLog :> es) => Eff es (Seq Page.Page)
paginateAll = do
  mainVList <- extractMainVListImpl
  desiredHeight <- HSt.getParameterValue (HSt.Param.LengthQuantParam HSt.Param.VSize)
  let naturalHeight = ListElem.vListNaturalHeight mainVList
  Log.infoLog $
    F.sformat
      ("desired height: " |%| Q.fmtLengthWithUnit |%| ", natural height: " |%| Q.fmtLengthWithUnit)
      desiredHeight
      naturalHeight
  Page.runPageBuilder desiredHeight mainVList

fmtPages :: Fmt (Seq Page.Page)
fmtPages = F.later $ \pages ->
  F.bformat (F.intercalated "\n\n" fmtPageWithIndex) (indexed (toList pages))
  where
    fmtPageWithIndex = F.later $ \(index, page) ->
      F.bformat (("Page " |%| F.accessed fst F.int |%| "\n=======\n") <> F.accessed snd Page.fmtPage) (index, page)
