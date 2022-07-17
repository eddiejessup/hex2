module Hex.Stage.Build.BoxElem where

import Formatting qualified as F
import Hex.Common.Box qualified as Box
import Hex.Common.DVI.Instruction qualified as DVI
import Hex.Common.HexState.Interface.Font qualified as HSt.Font
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
      ElemFontDefinition _fontDefinition ->
        Q.zeroLength
      ElemFontSelection _fontSelection ->
        Q.zeroLength
      ElemKern kern ->
        kern.unKern
    BoxGlue setGlue ->
      setGlue.sgDimen
  HBoxHBaseElem hBaseElem -> case hBaseElem of
    ElemCharacter charBox ->
      charBox.unCharacter.boxWidth

hBoxElemNaturalDepth :: HBoxElem -> Q.Length
hBoxElemNaturalDepth = \case
  HVBoxElem vBoxElem -> case vBoxElem of
    VBoxBaseElem baseElem -> case baseElem of
      ElemBox box ->
        box.unBaseBox.boxDepth
      ElemFontDefinition _fontDefinition ->
        Q.zeroLength
      ElemFontSelection _fontSelection ->
        Q.zeroLength
      ElemKern _kern ->
        Q.zeroLength
    BoxGlue _setGlue ->
      Q.zeroLength
  HBoxHBaseElem hBaseElem -> case hBaseElem of
    ElemCharacter charBox ->
      charBox.unCharacter.boxDepth

hBoxElemNaturalHeight :: HBoxElem -> Q.Length
hBoxElemNaturalHeight = \case
  HVBoxElem vBoxElem -> case vBoxElem of
    VBoxBaseElem baseElem -> case baseElem of
      ElemBox box ->
        box.unBaseBox.boxHeight
      ElemFontDefinition _fontDefinition ->
        Q.zeroLength
      ElemFontSelection _fontSelection ->
        Q.zeroLength
      ElemKern _kern ->
        Q.zeroLength
    BoxGlue _setGlue ->
      Q.zeroLength
  HBoxHBaseElem hBaseElem -> case hBaseElem of
    ElemCharacter charBox ->
      charBox.unCharacter.boxHeight

fmtHBoxElem :: Fmt HBoxElem
fmtHBoxElem = F.later $ \case
  HVBoxElem vBoxElem -> bformat fmtVBoxElemOneLine vBoxElem
  HBoxHBaseElem e -> bformat fmtHBaseElem e

-- TODO: Ligature, DiscretionaryBreak, Math on/off, V-adust
newtype HBaseElem
  = ElemCharacter Box.CharBox
  deriving stock (Show)

fmtHBaseElem :: Fmt HBaseElem
fmtHBaseElem = F.later $ \case
  ElemCharacter c -> bformat Box.fmtCharBoxWithDimens c

data VBoxElem
  = VBoxBaseElem BaseElem
  | BoxGlue SetGlue
  deriving stock (Show, Generic)

fmtVBoxElemOneLine :: Fmt VBoxElem
fmtVBoxElemOneLine = F.later $ \case
  VBoxBaseElem e -> bformat fmtBaseElemOneLine e
  BoxGlue sg -> bformat fmtSetGlue sg

data BaseElem
  = ElemBox BaseBox
  | ElemFontDefinition DVI.FontDefinition
  | ElemFontSelection HSt.Font.FontNumber
  | ElemKern Kern
  deriving stock (Show, Generic)

fmtBaseElemOneLine :: Fmt BaseElem
fmtBaseElemOneLine = F.later $ \case
  ElemBox b -> bformat fmtBaseBox b
  ElemFontDefinition _ ->
    "font-definition"
  ElemFontSelection _ ->
    "font-selection"
  ElemKern _ ->
    "kern"

newtype Kern = Kern {unKern :: Q.Length}
  deriving stock (Show, Eq, Generic)

-- Element constituents.

newtype SetGlue = SetGlue {sgDimen :: Q.Length}
  deriving stock (Show, Generic)

fmtSetGlue :: Fmt SetGlue
fmtSetGlue = F.fconst "\\setglue " <> fmtViewed #sgDimen Q.fmtLengthWithUnit

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
