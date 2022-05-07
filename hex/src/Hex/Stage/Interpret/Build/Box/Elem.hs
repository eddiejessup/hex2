module Hex.Stage.Interpret.Build.Box.Elem where

import Formatting qualified as F
import Hex.Common.Codes qualified as Codes
import Hex.Common.Quantity qualified as Q
import Hex.Common.HexState.Interface.Resolve.PrimitiveToken qualified as PT
import Hexlude

-- Box elements.

data HBoxElem
  = HVBoxElem VBoxElem
  | HBoxHBaseElem HBaseElem
  deriving stock (Show, Generic)

fmtHBoxElem :: Fmt HBoxElem
fmtHBoxElem = F.later $ \case
  HVBoxElem vBoxElem -> bformat fmtVBoxElemOneLine vBoxElem
  HBoxHBaseElem e -> bformat fmtHBaseElem e

-- TODO: Ligature, DiscretionaryBreak, Math on/off, V-adust
newtype HBaseElem
  = ElemCharacter CharBox
  deriving stock (Show)

fmtHBaseElem :: Fmt HBaseElem
fmtHBaseElem = F.later $ \case
  ElemCharacter c -> bformat fmtCharBoxWithDimens c

data VBoxElem
  = VBoxBaseElem BaseElem
  | BoxGlue SetGlue
  deriving stock (Show, Generic)

fmtVBoxElemOneLine :: Fmt VBoxElem
fmtVBoxElemOneLine = F.later $ \case
  VBoxBaseElem e -> bformat fmtBaseElemOneLine e
  BoxGlue sg -> bformat fmtSetGlue sg

data BaseElem
  = ElemBox (Box BaseBoxContents)
  | ElemFontDefinition FontDefinition
  | ElemFontSelection FontSelection
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

-- Boxes.

data Box a = Box {contents :: a, boxWidth, boxHeight, boxDepth :: Q.Length}
  deriving stock (Show, Eq, Generic, Functor, Foldable)

fmtBoxDimens :: Fmt (Box a)
fmtBoxDimens =
  let fmtWidth = fmtViewed #boxWidth Q.fmtLengthMagnitude
      fmtHeight = fmtViewed #boxHeight Q.fmtLengthMagnitude
      fmtDepth = fmtViewed #boxDepth Q.fmtLengthMagnitude
   in F.squared $ F.fconst "⇿" <> fmtWidth <> F.fconst "↥" <> fmtHeight <> F.fconst "↧" <> fmtDepth

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

fmtBaseBox :: Fmt (Box BaseBoxContents)
fmtBaseBox = fmtBoxDimens <> fmtViewed #contents fmtBaseBoxContents

newtype HBoxElemSeq = HBoxElemSeq {unHBoxElemSeq :: Seq HBoxElem}
  deriving stock (Show, Generic)

newtype VBoxElemSeq = VBoxElemSeq {unVBoxElemSeq :: Seq VBoxElem}
  deriving stock (Show, Generic)

newtype Rule = Rule {unRule :: Box ()}
  deriving stock (Show, Eq, Generic)

newtype CharBox = CharBox {unCharacter :: Box Codes.CharCode}
  deriving stock (Show, Generic)

fmtCharBox :: Fmt CharBox
fmtCharBox = fmtViewed (to charBoxChar) (F.squoted F.char)

fmtCharBoxWithDimens :: Fmt CharBox
fmtCharBoxWithDimens = fmtViewed #unCharacter fmtBoxDimens <> F.prefixed "\\c" fmtCharBox

charBoxChar :: CharBox -> Char
charBoxChar = view $ #unCharacter % #contents % to Codes.unsafeCodeAsChar

newtype FontSelection = FontSelection PT.FontNumber
  deriving stock (Show, Generic)

data FontDefinition = FontDefinition
  { fontDefChecksum :: Int,
    fontDefDesignSize :: Q.Length,
    fontDefDesignScale :: Q.Length,
    fontPath :: HexFilePath,
    fontName :: Text,
    fontNr :: PT.FontNumber
  }
  deriving stock (Show, Generic)

data FontSpecification = NaturalFont | FontAt Q.Length | FontScaled Q.HexInt
  deriving stock (Show, Eq, Generic)

newtype HexFilePath = HexFilePath FilePath
  deriving stock (Show, Eq, Generic)
