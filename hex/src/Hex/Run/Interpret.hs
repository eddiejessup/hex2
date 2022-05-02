module Hex.Run.Interpret where

import Hex.Run.App (App)
import Hex.Common.HexState.Impl ()
import Hex.Stage.Categorise.Impl ()
import Hex.Stage.Lex.Impl ()
import Hex.Stage.Resolve.Impl ()
import Hex.Stage.Expand.Impl ()
import Hex.Stage.Parse.Impl ()
import Hex.Stage.Evaluate.Impl ()
import Hex.Stage.Interpret.Build.List.Elem (VList(..))
import Hex.Stage.Interpret.CommandHandler.MainVMode (buildMainVList)

interpretAll :: App VList
interpretAll = buildMainVList
