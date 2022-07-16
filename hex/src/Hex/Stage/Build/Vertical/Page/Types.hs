module Hex.Stage.Build.Vertical.Page.Types where

import Formatting qualified as F
import Hex.Stage.Build.BoxElem qualified as B
import Hexlude

newtype Page = Page {unPage :: B.VBoxElemSeq}
  deriving stock (Show, Generic)

fmtPage :: Fmt Page
fmtPage = F.accessed (.unPage) B.fmtVBoxElemSeq
