module Hex.Common.Box where

import Formatting qualified as F
import Hex.Common.Codes qualified as Codes
import Hex.Common.Quantity qualified as Q
import Hexlude

data BoxDim
  = BoxWidth
  | BoxHeight
  | BoxDepth
  deriving stock (Show, Eq, Generic)

data Box a = Box {contents :: a, boxWidth, boxHeight, boxDepth :: Q.Length}
  deriving stock (Show, Eq, Generic, Functor, Foldable)

boxSpanAlongAxis :: Axis -> Box a -> Q.Length
boxSpanAlongAxis ax b = case ax of
  Vertical -> boxHeightAndDepth b
  Horizontal -> b.boxWidth

boxHeightAndDepth :: Box a -> Q.Length
boxHeightAndDepth b = b.boxHeight <> b.boxDepth

fmtBoxDimens :: Fmt (Box a)
fmtBoxDimens =
  let fmtWidth = fmtViewed #boxWidth Q.fmtLengthWithUnit
      fmtHeight = fmtViewed #boxHeight Q.fmtLengthWithUnit
      fmtDepth = fmtViewed #boxDepth Q.fmtLengthWithUnit
   in F.squared $ F.fconst "⇿" <> fmtWidth <> F.fconst "↥" <> fmtHeight <> F.fconst "↧" <> fmtDepth

newtype Rule = Rule {unRule :: Box ()}
  deriving stock (Show, Eq, Generic)

fmtRule :: Fmt Rule
fmtRule = F.accessed (.unRule) fmtBoxDimens |%| "\\rule{}"

newtype CharBox = CharBox {unCharacter :: Box Codes.CharCode}
  deriving stock (Show, Generic)

fmtCharBox :: Fmt CharBox
fmtCharBox = fmtViewed (to charBoxChar) (F.squoted F.char)

fmtCharBoxWithDimens :: Fmt CharBox
fmtCharBoxWithDimens = fmtViewed #unCharacter fmtBoxDimens <> F.prefixed "\\c" fmtCharBox

charBoxChar :: CharBox -> Char
charBoxChar = view $ #unCharacter % #contents % to Codes.unsafeCodeAsChar
