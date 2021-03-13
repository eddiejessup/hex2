module Hex.Interpret.Build.Box.Elem where

import Hex.Codes qualified as H.Codes
import Hex.Quantity qualified as H.Q
import Hexlude

-- Box elements.

data HBoxElem
  = HVBoxElem VBoxElem
  | HBoxHBaseElem HBaseElem
  deriving stock (Show, Generic)

-- TODO: Ligature, DiscretionaryBreak, Math on/off, V-adust
newtype HBaseElem
  = ElemCharacter Character
  deriving stock (Show)

data VBoxElem
  = VBoxBaseElem BaseElem
  | BoxGlue (SetGlue H.Q.Length)
  deriving stock (Show, Generic)

data BaseElem
  = ElemBox (Box BoxContents)
  | ElemRule Rule
  | ElemFontDefinition FontDefinition
  | ElemFontSelection FontSelection
  | ElemKern Kern
  deriving stock (Show, Generic)

-- Boxes.

newtype HBox = HBox (Seq HBoxElem)
  deriving stock (Show, Generic)
  deriving newtype (Semigroup, Monoid)

newtype VBox = VBox (Seq VBoxElem)
  deriving stock (Show, Generic)
  deriving newtype (Semigroup, Monoid)

data Box a = Box {contents :: a, boxWidth, boxHeight, boxDepth :: H.Q.Length}
  deriving stock (Show, Generic, Functor, Foldable)

newtype Page = Page (Box VBox)
  deriving stock (Show)

-- Element constituents.

newtype SetGlue a = SetGlue {glueDimen :: a}
  deriving stock (Show, Generic)

data BoxContents
  = HBoxContents HBox
  | VBoxContents VBox
  deriving stock (Show, Generic)

newtype Rule = Rule (Box ())
  deriving stock (Show, Generic)

newtype Kern = Kern {kernDimen :: H.Q.Length}
  deriving stock (Show)

newtype Character = Character (Box H.Codes.CharCode)
  deriving stock (Show, Generic)

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
