module Hex.Interpret.Build.Box.Elem where

import Formatting qualified as F
import Hex.Codes qualified as H.Codes
import Hex.Quantity qualified as H.Q
import Hexlude

-- Box elements.

data HBoxElem
  = HVBoxElem VBoxElem
  | HBoxHBaseElem HBaseElem
  deriving stock (Show, Generic)

fmtHBoxElem :: Fmt HBoxElem r
fmtHBoxElem = F.later $ \case
  HVBoxElem vBoxElem -> bformat fmtVBoxElemOneLine vBoxElem
  HBoxHBaseElem e -> bformat fmtHBaseElem e

-- TODO: Ligature, DiscretionaryBreak, Math on/off, V-adust
newtype HBaseElem
  = ElemCharacter CharBox
  deriving stock (Show)

fmtHBaseElem :: Fmt HBaseElem r
fmtHBaseElem = F.later $ \case
  ElemCharacter c -> bformat fmtCharBoxWithDimens c

data VBoxElem
  = VBoxBaseElem BaseElem
  | BoxGlue SetGlue
  deriving stock (Show, Generic)

fmtVBoxElemOneLine :: Fmt VBoxElem r
fmtVBoxElemOneLine = F.later $ \case
  VBoxBaseElem e -> bformat fmtBaseElemOneLine e
  BoxGlue sg -> bformat fmtSetGlue sg

data BaseElem
  = ElemBox (Box BaseBoxContents)
  | ElemFontDefinition FontDefinition
  | ElemFontSelection FontSelection
  | ElemKern Kern
  deriving stock (Show, Generic)

fmtBaseElemOneLine :: Fmt BaseElem r
fmtBaseElemOneLine = F.later $ \case
  ElemBox b -> bformat fmtBaseBox b
  ElemFontDefinition _ ->
    "font-definition"
  ElemFontSelection _ ->
    "font-selection"
  ElemKern _ ->
    "kern"

-- Boxes.

data Box a = Box {contents :: a, boxWidth, boxHeight, boxDepth :: H.Q.Length}
  deriving stock (Show, Generic, Functor, Foldable)

fmtBoxDimens :: Fmt (Box a) r
fmtBoxDimens =
  let fmtWidth = fmtViewed #boxWidth H.Q.fmtLengthMagnitude
      fmtHeight = fmtViewed #boxHeight H.Q.fmtLengthMagnitude
      fmtDepth = fmtViewed #boxDepth H.Q.fmtLengthMagnitude
   in F.squared $ F.fconst "⇿" <> fmtWidth <> F.fconst "↥" <> fmtHeight <> F.fconst "↧" <> fmtDepth

-- Element constituents.

newtype SetGlue = SetGlue {sgDimen :: H.Q.Length}
  deriving stock (Show, Generic)

fmtSetGlue :: Fmt SetGlue r
fmtSetGlue = F.fconst "\\setglue " <> fmtViewed #sgDimen H.Q.fmtLengthWithUnit

data BaseBoxContents
  = HBoxContents HBoxElemSeq
  | VBoxContents VBoxElemSeq
  | RuleContents
  deriving stock (Show, Generic)

fmtBaseBoxContents :: Fmt BaseBoxContents r
fmtBaseBoxContents = F.later $ \case
  HBoxContents hboxElemSeq -> bformat (F.prefixed "\\hbox" $ fmtViewed #unHBoxElemSeq (F.braced (F.commaSpaceSep fmtHBoxElem))) hboxElemSeq
  VBoxContents vboxElemSeq -> bformat (F.prefixed "\\vbox" $ fmtViewed #unVBoxElemSeq (F.braced (F.commaSpaceSep fmtVBoxElemOneLine))) vboxElemSeq
  RuleContents -> "\\rule{}"

fmtBaseBox :: Fmt (Box BaseBoxContents) r
fmtBaseBox = fmtBoxDimens <> fmtViewed #contents fmtBaseBoxContents

newtype Kern = Kern {unKern :: H.Q.Length}
  deriving stock (Show, Generic)

newtype HBoxElemSeq = HBoxElemSeq {unHBoxElemSeq :: Seq HBoxElem}
  deriving stock (Show, Generic)

newtype VBoxElemSeq = VBoxElemSeq {unVBoxElemSeq :: Seq VBoxElem}
  deriving stock (Show, Generic)

newtype Rule = Rule {unRule :: Box ()}
  deriving stock (Show, Generic)

newtype CharBox = CharBox {unCharacter :: Box H.Codes.CharCode}
  deriving stock (Show, Generic)

fmtCharBox :: Fmt CharBox r
fmtCharBox = fmtViewed (to charBoxChar) (F.squoted F.char)

fmtCharBoxWithDimens :: Fmt CharBox r
fmtCharBoxWithDimens = fmtViewed #unCharacter fmtBoxDimens <> F.prefixed "\\c" fmtCharBox

charBoxChar :: CharBox -> Char
charBoxChar = view $ #unCharacter % #contents % to H.Codes.unsafeCodeAsChar

data FontDefinition = FontDefinition
  { fontDefChecksum :: Int,
    fontDefDesignSize :: H.Q.Length,
    fontDefDesignScale :: H.Q.Length,
    fontPath :: FilePath,
    fontName :: Text,
    fontNr :: H.Q.HexInt
  }
  deriving stock (Show, Generic)

newtype FontSelection = FontSelection H.Q.HexInt
  deriving stock (Show, Generic)
