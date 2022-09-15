module Hex.Stage.Build.BoxElem where

import Formatting qualified as F
import Hex.Common.Box qualified as Box
import Hex.Common.Quantity qualified as Q
import Hexlude

-- Box elements.

data HBoxElem
  = HVBoxElem VBoxElem
  | HBoxHBaseElem HBaseElem
  deriving stock (Show, Generic)

hBoxElemNaturalWidth :: HBoxElem -> Q.Length
hBoxElemNaturalWidth = \case
  HVBoxElem vBoxElem -> case vBoxElem of
    VBoxBaseElem baseElem -> case baseElem of
      ElemBox box ->
        box.unBaseBox.boxWidth
      ElemKern kern ->
        kern.unKern
  HBoxHBaseElem hBaseElem -> case hBaseElem of
    ElemCharacter charBox ->
      charBox.unCharBox.boxWidth

hBoxElemNaturalDepth :: HBoxElem -> Q.Length
hBoxElemNaturalDepth = \case
  HVBoxElem vBoxElem -> case vBoxElem of
    VBoxBaseElem baseElem -> case baseElem of
      ElemBox box ->
        box.unBaseBox.boxDepth
      ElemKern _kern ->
        Q.zeroLength
  HBoxHBaseElem hBaseElem -> case hBaseElem of
    ElemCharacter charBox ->
      charBox.unCharBox.boxDepth

hBoxElemNaturalHeight :: HBoxElem -> Q.Length
hBoxElemNaturalHeight = \case
  HVBoxElem vBoxElem -> case vBoxElem of
    VBoxBaseElem baseElem -> case baseElem of
      ElemBox box ->
        box.unBaseBox.boxHeight
      ElemKern _kern ->
        Q.zeroLength
  HBoxHBaseElem hBaseElem -> case hBaseElem of
    ElemCharacter charBox ->
      charBox.unCharBox.boxHeight

fmtHBoxElem :: Fmt HBoxElem
fmtHBoxElem = F.later $ \case
  HVBoxElem vBoxElem -> bformat fmtVBoxElemOneLine vBoxElem
  HBoxHBaseElem e -> bformat fmtHBaseElem e

-- TODO: Ligature, Math on/off, V-adust
newtype HBaseElem
  = ElemCharacter Box.CharBox
  deriving stock (Show)

fmtHBaseElem :: Fmt HBaseElem
fmtHBaseElem = F.later $ \case
  ElemCharacter c -> bformat Box.fmtCharBoxWithDimens c

data VBoxElem
  = VBoxBaseElem BaseElem
  deriving stock (Show, Generic)

fmtVBoxElemOneLine :: Fmt VBoxElem
fmtVBoxElemOneLine = F.later $ \case
  VBoxBaseElem e -> bformat fmtBaseElemOneLine e

data BaseElem
  = ElemBox BaseBox
  | ElemKern Kern
  deriving stock (Show, Generic)

fmtBaseElemOneLine :: Fmt BaseElem
fmtBaseElemOneLine = F.later $ \case
  ElemBox b -> bformat fmtBaseBox b
  ElemKern _ ->
    "kern"

newtype Kern = Kern {unKern :: Q.Length}
  deriving stock (Show, Eq, Generic)

fmtKern :: Fmt Kern
fmtKern = F.accessed (.unKern) ("K" |%| Q.fmtLengthWithUnit)

-- Element constituents.

data BaseBoxContents
  = HBoxContents HBoxElemSeq
  | VBoxContents VBoxElemSeq
  | RuleContents
  deriving stock (Show, Generic)

fmtBaseBoxContents :: Fmt BaseBoxContents
fmtBaseBoxContents = F.later $ \case
  HBoxContents hboxElemSeq -> bformat (F.prefixed "\\hbox" $ fmtViewed #unHBoxElemSeq (F.braced (F.commaSpaceSep fmtHBoxElem))) hboxElemSeq
  VBoxContents vboxElemSeq -> bformat (F.prefixed "\\vbox" $ fmtViewed #unVBoxElemSeq (F.braced (F.commaSpaceSep fmtVBoxElemOneLine))) vboxElemSeq
  RuleContents -> "\\rule{}"

newtype BaseBox = BaseBox {unBaseBox :: Box.Box BaseBoxContents}
  deriving stock (Show, Generic)

fmtBaseBox :: Fmt BaseBox
fmtBaseBox = F.accessed (.unBaseBox) (Box.fmtBoxDimens <> F.accessed (.contents) fmtBaseBoxContents)

ruleAsBaseBox :: Box.Rule -> BaseBox
ruleAsBaseBox (Box.Rule box) = BaseBox $ box <&> \() -> RuleContents

newtype HBoxElemSeq = HBoxElemSeq {unHBoxElemSeq :: Seq HBoxElem}
  deriving stock (Show, Generic)
  deriving newtype (Semigroup, Monoid)

singletonHBoxElemSeq :: HBoxElem -> HBoxElemSeq
singletonHBoxElemSeq = HBoxElemSeq . pure

fmtHBoxElemSeq :: Fmt HBoxElemSeq
fmtHBoxElemSeq = F.accessed (.unHBoxElemSeq) (F.intercalated "\n" fmtHBoxElem)

hBoxElemTraversal :: Traversal' HBoxElemSeq HBoxElem
hBoxElemTraversal = #unHBoxElemSeq % traversed

hBoxNaturalDepth :: HBoxElemSeq -> Q.Length
hBoxNaturalDepth hBox =
  -- The empty HBox has zero depth.
  fromMaybe Q.zeroLength $
    maximumOf (hBoxElemTraversal % to hBoxElemNaturalDepth) hBox

hBoxNaturalHeight :: HBoxElemSeq -> Q.Length
hBoxNaturalHeight hBox =
  -- The empty HBox has zero height.
  fromMaybe Q.zeroLength $
    maximumOf (hBoxElemTraversal % to hBoxElemNaturalHeight) hBox

hBoxNaturalWidth :: HBoxElemSeq -> Q.Length
hBoxNaturalWidth =
  foldMapOf hBoxElemTraversal hBoxElemNaturalWidth

newtype VBoxElemSeq = VBoxElemSeq {unVBoxElemSeq :: Seq VBoxElem}
  deriving stock (Show, Generic)

fmtVBoxElemSeq :: Fmt VBoxElemSeq
fmtVBoxElemSeq = F.accessed (.unVBoxElemSeq) (F.intercalated "\n" fmtVBoxElemOneLine)

vBoxElemTraversal :: Traversal' VBoxElemSeq VBoxElem
vBoxElemTraversal = #unVBoxElemSeq % traversed
