module Hex.Stage.Build.Vertical.Page.Types where

import Formatting qualified as F
import Hex.Stage.Build.BoxElem qualified as BoxElem
import Hexlude

newtype Page = Page {unPage :: Seq BoxElem.VBoxElem}
  deriving stock (Show, Generic)

fmtPage :: Fmt Page
fmtPage = F.accessed (.unPage) BoxElem.fmtVBoxElemSeq
