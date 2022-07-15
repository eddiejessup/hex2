module Hex.Run.Interpret where

import Hex.Run.App (App)
import Hex.Stage.Build.ListElem (HList (..), VList (..))
import Hex.Stage.Build.ListExtractor.Interface (EndHListReason, IndentFlag (..), extractParagraphList)
import Hex.Stage.Build.ListExtractor.VList (extractMainVListImpl)

interpretInMainVMode :: App VList
interpretInMainVMode = extractMainVListImpl

interpretInParaMode :: App (EndHListReason, HList)
interpretInParaMode = extractParagraphList Indent
