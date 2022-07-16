module Hex.Run.Paginate where

import Formatting qualified as F
import Hex.Capability.Log.Interface qualified as Log
import Hex.Common.HexState.Interface qualified as HSt
import Hex.Common.HexState.Interface.Parameter qualified as HSt.Param
import Hex.Common.Quantity qualified as Q
import Hex.Run.App (App)
import Hex.Stage.Build.ListExtractor.VList (extractMainVListImpl)
import Hex.Stage.Build.Vertical.Page.Break qualified as Page
import Hex.Stage.Build.Vertical.Page.Types qualified as Page
import Hexlude

paginateAll :: App (Seq Page.Page)
paginateAll = do
  mainVList <- extractMainVListImpl
  desiredHeight <- HSt.getParameterValue (HSt.Param.LengthQuantParam HSt.Param.VSize)
  Log.infoLog $ "desiredHeight: " <> F.sformat Q.fmtLengthWithUnit desiredHeight
  Page.runPageBuilder desiredHeight mainVList

fmtPages :: Fmt (Seq Page.Page)
fmtPages = F.later $ \pages ->
  F.bformat (F.intercalated "\n\n" fmtPageWithIndex) (indexed (toList pages))
  where
    fmtPageWithIndex = F.later $ \(index, page) ->
      F.bformat (("Page " |%| F.accessed fst F.int |%| "\n=======\n") <> F.accessed snd Page.fmtPage) (index, page)
