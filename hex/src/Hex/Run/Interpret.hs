module Hex.Run.Interpret where

import Hex.Common.HexState.Interface.Resolve.PrimitiveToken qualified as PT
import Hex.Run.App (App)
import Hex.Stage.Interpret.Build.List.Elem (HList (..), VList (..))
import Hex.Stage.Interpret.CommandHandler.MainVMode (buildMainVList)
import Hex.Stage.Interpret.CommandHandler.ParaMode (EndParaReason, buildParaList)

interpretInMainVMode :: App VList
interpretInMainVMode = buildMainVList

interpretInParaMode :: App (EndParaReason, HList)
interpretInParaMode = buildParaList PT.Indent
