module Hex.Run.Interpret where

import Hex.Run.App (App)
import Hex.Stage.Build.ListExtractor.Interface (extractParagraphList, EndHListReason, IndentFlag (..))
import Hex.Stage.Build.ListExtractor.VList (extractMainVListImpl)
import Hex.Stage.Build.ListElem (HList (..), VList (..))

interpretInMainVMode :: App VList
interpretInMainVMode = extractMainVListImpl

interpretInParaMode :: App (EndHListReason, HList)
interpretInParaMode = extractParagraphList Indent
