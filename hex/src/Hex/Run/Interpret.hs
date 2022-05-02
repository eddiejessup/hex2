module Hex.Run.Interpret where

import Hex.Common.HexState.Impl ()
import Hex.Common.HexState.Interface.Resolve.PrimitiveToken qualified as PT
import Hex.Run.App (App)
import Hex.Stage.Categorise.Impl ()
import Hex.Stage.Evaluate.Impl ()
import Hex.Stage.Expand.Impl ()
import Hex.Stage.Interpret.Build.List.Elem (HList (..), VList (..))
import Hex.Stage.Interpret.CommandHandler.MainVMode (buildMainVList)
import Hex.Stage.Interpret.CommandHandler.ParaMode (EndParaReason, buildParaList)
import Hex.Stage.Lex.Impl ()
import Hex.Stage.Parse.Impl ()
import Hex.Stage.Resolve.Impl ()

interpretInMainVMode :: App VList
interpretInMainVMode = buildMainVList

interpretInParaMode :: App (EndParaReason, HList)
interpretInParaMode = buildParaList PT.Indent
