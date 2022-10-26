module Hex.Common.Box where

import Formatting qualified as F
import Hex.Common.Quantity qualified as Q
import Hexlude

data BoxDim
  = BoxWidth
  | BoxHeight
  | BoxDepth
  deriving stock (Show, Eq, Generic)

data BoxDims a = BoxDims {boxWidth, boxHeight, boxDepth :: a}
  deriving stock (Show, Eq, Generic, Functor)

data Boxed a = Boxed {boxedContents :: a, boxedDims :: BoxDims Q.Length}
  deriving stock (Show, Eq, Generic, Functor, Foldable)

fmtBoxed :: Fmt a -> Fmt (Boxed a)
fmtBoxed fmt = F.accessed (.boxedDims) fmtBoxDimens <> F.accessed (.boxedContents) fmt

boxSpanAlongAxis :: Semigroup a => Axis -> BoxDims a -> a
boxSpanAlongAxis ax b = case ax of
  Vertical -> boxHeightAndDepth b
  Horizontal -> b.boxWidth

boxHeightAndDepth :: Semigroup a => BoxDims a -> a
boxHeightAndDepth b = b.boxHeight <> b.boxDepth

fmtBoxDimens :: Fmt (BoxDims Q.Length)
fmtBoxDimens =
  let fmtWidth = fmtViewed #boxWidth Q.fmtLengthWithUnit
      fmtHeight = fmtViewed #boxHeight Q.fmtLengthWithUnit
      fmtDepth = fmtViewed #boxDepth Q.fmtLengthWithUnit
   in F.squared $ F.fconst "⇿" <> fmtWidth <> F.fconst "↥" <> fmtHeight <> F.fconst "↧" <> fmtDepth

data OffsetInDirection a = OffsetInDirection Direction a
  deriving stock (Show, Eq, Generic, Functor, Foldable, Traversable)

offsetForward :: Group a => OffsetInDirection a -> a
offsetForward (OffsetInDirection Forward a) = a
offsetForward (OffsetInDirection Backward a) = invert a

fmtOffsetInDirection :: Fmt a -> Fmt (OffsetInDirection a)
fmtOffsetInDirection fmtLen = F.later $ \case
  OffsetInDirection Forward len -> F.bformat ("<< " |%| fmtLen |%| " <<") len
  OffsetInDirection Backward len -> F.bformat (">> " |%| fmtLen |%| " >>") len

data Offsettable a b = Offsettable {offset :: Maybe (OffsetInDirection a), offsetContents :: b}
  deriving stock (Show, Eq, Generic)

fmtOffsettable :: Fmt a -> Fmt b -> Fmt (Offsettable a b)
fmtOffsettable fmtLen fmtContents =
  F.squared (F.accessed (.offset) (F.maybed "" (fmtOffsetInDirection fmtLen)))
    <> F.accessed (.offsetContents) fmtContents
