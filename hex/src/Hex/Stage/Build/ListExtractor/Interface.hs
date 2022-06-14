module Hex.Stage.Build.ListExtractor.Interface where

import Hex.Stage.Build.ListElem qualified as B
import Hexlude

data IndentFlag
  = Indent
  | DoNotIndent
  deriving stock (Show, Eq, Generic)

data EndHListReason
  = EndHListSawEndParaCommand
  | EndHListSawLeaveBox
  deriving stock (Show, Generic)

data ModeContext = InnerModeContext | OuterModeContext
  deriving stock (Show, Generic)

class Monad m => MonadHexListExtractor m where
  extractHBoxList :: m B.HList

  extractParagraphList :: IndentFlag -> m (EndHListReason, B.HList)
