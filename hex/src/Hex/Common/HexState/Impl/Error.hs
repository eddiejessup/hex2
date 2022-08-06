module Hex.Common.HexState.Impl.Error where

import Hexlude
import qualified Formatting as F

data HexStateError
  = FontNotFound HexFilePath
  | MissingFontNumber
  | BadPath Text
  | CharacterCodeNotFound
  | PoppedEmptyGroups
  | UnmatchedExitGroupTrigger
  | TriedToLeaveMainVMode
  deriving stock (Show, Generic)

fmtHexStateError :: Fmt HexStateError
fmtHexStateError = F.shown
