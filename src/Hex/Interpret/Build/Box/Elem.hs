module Hex.Interpret.Build.Box.Elem where

import Protolude
import Path qualified
import Hex.Interpret.Evaluate.Evaluated qualified as H.Inter.Eval
import Hex.Codes qualified as H.Codes

-- Box elements.

data HBoxElem
  = HVBoxElem VBoxElem
  | HBoxHBaseElem HBaseElem
  deriving stock (Show, Generic)

-- TODO: Ligature, DiscretionaryBreak, Math on/off, V-adust
newtype HBaseElem
  = ElemCharacter Character
  deriving stock Show

data VBoxElem
  = VBoxBaseElem BaseElem
  | BoxGlue (SetGlue H.Inter.Eval.Length)
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

data Box a = Box {contents :: a, boxWidth, boxHeight, boxDepth :: H.Inter.Eval.Length}
  deriving stock (Show, Generic, Functor, Foldable)

newtype Page = Page (Box VBox)
  deriving stock Show

-- Element constituents.

newtype SetGlue a = SetGlue {glueDimen :: a}
  deriving stock (Show, Generic)

data BoxContents
  = HBoxContents HBox
  | VBoxContents VBox
  deriving stock (Show, Generic)

data Rule = Rule {ruleWidth, ruleHeight, ruleDepth :: H.Inter.Eval.Length}
  deriving stock (Show, Generic)

newtype Kern = Kern {kernDimen :: H.Inter.Eval.Length}
  deriving stock Show

data Character
  = Character {char :: H.Codes.CharCode, charWidth, charHeight, charDepth :: H.Inter.Eval.Length}
  deriving stock (Show, Generic)

data FontDefinition
  = FontDefinition
      { fontDefChecksum :: Int
      , fontDefDesignSize :: H.Inter.Eval.Length
      , fontDefDesignScale :: H.Inter.Eval.Length
      , fontPath :: Path.Path Path.Rel Path.File
      , fontName :: Text
      , fontNr :: H.Inter.Eval.HexInt
      }
  deriving stock (Show, Generic)

newtype FontSelection = FontSelection H.Inter.Eval.HexInt
  deriving stock (Show, Generic)
