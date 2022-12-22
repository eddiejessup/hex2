{-# LANGUAGE NoImpredicativeTypes #-}

module Hex.Stage.Build.BoxElem where

import Data.Text.Lazy.Builder (Builder)
import Formatting qualified as F
import Hex.Common.Box qualified as Box
import Hex.Common.Codes qualified as Codes
import Hex.Common.Font qualified as Font
import Hex.Common.Quantity qualified as Q
import Hexlude

-- Box elements.

data HBoxElem
  = HVBoxElem VBoxElem
  | HBoxHBaseElem HBaseElem
  deriving stock (Show, Eq, Generic)

hBoxElemNaturalWidth :: HBoxElem -> Q.Length
hBoxElemNaturalWidth = \case
  HVBoxElem vBoxElem -> case vBoxElem of
    VBoxBaseElem baseElem -> case baseElem of
      AxOrRuleBoxBaseElem box ->
        box.boxedDims.boxWidth
      KernBaseElem kern ->
        kern.unKern
    VBoxSetGlueElem setGlue ->
      setGlue.glue.gDimen
  HBoxHBaseElem hBaseElem -> case hBaseElem of
    CharBoxHBaseElem charBox ->
      charBox.boxedDims.boxWidth

hBoxElemNaturalDepth :: HBoxElem -> Q.Length
hBoxElemNaturalDepth = \case
  HVBoxElem vBoxElem -> case vBoxElem of
    VBoxBaseElem baseElem -> case baseElem of
      AxOrRuleBoxBaseElem box ->
        box.boxedDims.boxDepth
      KernBaseElem _kern ->
        Q.zeroLength
    VBoxSetGlueElem _setGlue ->
      Q.zeroLength
  HBoxHBaseElem hBaseElem -> case hBaseElem of
    CharBoxHBaseElem charBox ->
      charBox.boxedDims.boxDepth

hBoxElemNaturalHeight :: HBoxElem -> Q.Length
hBoxElemNaturalHeight = \case
  HVBoxElem vBoxElem -> case vBoxElem of
    VBoxBaseElem baseElem -> case baseElem of
      AxOrRuleBoxBaseElem box ->
        box.boxedDims.boxHeight
      KernBaseElem _kern ->
        Q.zeroLength
    VBoxSetGlueElem _setGlue ->
      Q.zeroLength
  HBoxHBaseElem hBaseElem -> case hBaseElem of
    CharBoxHBaseElem charBox ->
      charBox.boxedDims.boxHeight

fmtHBoxElem :: Fmt HBoxElem
fmtHBoxElem = F.later $ \case
  HVBoxElem vBoxElem -> bformat fmtVBoxElemOneLine vBoxElem
  HBoxHBaseElem e -> bformat fmtHBaseElem e

-- TODO: Ligature, Math on/off, V-adust
newtype HBaseElem
  = CharBoxHBaseElem (Box.Boxed CharBoxContents)
  deriving stock (Show, Eq, Generic)

data CharBoxContents = CharBoxContents
  { charBoxCharCode :: Codes.CharCode,
    charBoxFont :: Font.FontNumber
  }
  deriving stock (Show, Eq, Generic)

fmtCharBoxContents :: Fmt CharBoxContents
fmtCharBoxContents = fmtViewed (to charBoxChar) (F.squoted F.char)

charBoxChar :: CharBoxContents -> Char
charBoxChar = Codes.unsafeCodeAsChar . (.charBoxCharCode)

fmtHBaseElem :: Fmt HBaseElem
fmtHBaseElem = F.later $ \case
  CharBoxHBaseElem c -> bformat (Box.fmtBoxed fmtCharBoxContents) c

data VBoxElem
  = VBoxBaseElem BaseElem
  | VBoxSetGlueElem SetGlue
  deriving stock (Show, Eq, Generic)

data SetGlue = SetGlue {setAt :: Q.Length, glue :: Q.Glue}
  deriving stock (Show, Eq, Generic)

-- >>> a = SetGlue {setAt = Q.onePt, glue = Q.filStretchGlue}
fmtSetGlue :: Fmt SetGlue
fmtSetGlue = F.accessed asKVs (F.prefixed "\\setglue" $ F.braced $ F.commaSpaceSep fmtKV)
  where
    asKVs :: SetGlue -> [(Text, Text)]
    asKVs SetGlue {setAt, glue} =
      [("setAt", F.sformat Q.fmtLengthWithUnit setAt), ("glue", F.sformat Q.fmtGlue glue)]

    fmtKV :: Fmt (Text, Text)
    fmtKV = F.accessed fst F.stext <> " = " |%| F.accessed snd F.stext

fmtVBoxElemOneLine :: Fmt VBoxElem
fmtVBoxElemOneLine = F.later $ \case
  VBoxBaseElem e -> bformat fmtBaseElemOneLine e
  VBoxSetGlueElem setGlue -> bformat fmtSetGlue setGlue

-- \setGlue{}

data BaseElem
  = AxOrRuleBoxBaseElem (Box.Boxed AxBoxOrRuleContents)
  | KernBaseElem Kern
  deriving stock (Show, Eq, Generic)

fmtBaseElemOneLine :: Fmt BaseElem
fmtBaseElemOneLine = F.later $ \case
  AxOrRuleBoxBaseElem b ->
    bformat (Box.fmtBoxed fmtAxOrRuleBoxContentsOneLine) b
  KernBaseElem k ->
    bformat fmtKern k

newtype Kern = Kern {unKern :: Q.Length}
  deriving stock (Show, Eq, Generic)

fmtKern :: Fmt Kern
fmtKern = F.accessed (.unKern) ("K" |%| Q.fmtLengthWithUnit)

-- Element constituents.

data AxBoxOrRuleContents
  = AxBoxOrRuleContentsRule
  | AxBoxOrRuleContentsAx (Box.Offsettable Q.Length AxBoxElems)
  deriving stock (Show, Eq, Generic)

fmtBoxedAxBoxElemsOneLine :: Fmt (Box.Boxed AxBoxElems)
fmtBoxedAxBoxElemsOneLine = Box.fmtBoxed fmtAxBoxElemsOneLine

fmtAxOrRuleBoxContentsOneLine :: Fmt AxBoxOrRuleContents
fmtAxOrRuleBoxContentsOneLine = F.later $ \case
  AxBoxOrRuleContentsRule -> "\\rule{}"
  AxBoxOrRuleContentsAx x@(Box.Offsettable _ (AxBoxElemsH _)) ->
    bformat
      (F.prefixed "\\hbox" $ Box.fmtOffsettable Q.fmtLengthWithUnit fmtAxBoxElemsOneLine)
      x
  AxBoxOrRuleContentsAx x@(Box.Offsettable _ (AxBoxElemsV _)) ->
    bformat
      (F.prefixed "\\hbox" $ Box.fmtOffsettable Q.fmtLengthWithUnit fmtAxBoxElemsOneLine)
      x

data AxBoxElems
  = AxBoxElemsH (Seq HBoxElem)
  | AxBoxElemsV (Seq VBoxElem)
  deriving stock (Show, Eq, Generic)

axBoxElemsAxis :: AxBoxElems -> Axis
axBoxElemsAxis = \case
  AxBoxElemsH _ -> Horizontal
  AxBoxElemsV _ -> Vertical

fmtAxBoxElemsOneLine :: Fmt AxBoxElems
fmtAxBoxElemsOneLine = F.later $ \case
  AxBoxElemsH h -> bformat (fmtSeqOneLine fmtHBoxElem) h
  AxBoxElemsV v -> bformat (fmtSeqOneLine fmtVBoxElemOneLine) v

fmtSeqLined :: Format Builder (a -> Builder) -> Format r (Seq a -> r)
fmtSeqLined = F.intercalated "\n"

fmtSeqOneLine :: Format Builder (a -> Builder) -> Format r (Seq a -> r)
fmtSeqOneLine = F.commaSpaceSep

singletonHBoxElemSeq :: HBoxElem -> Seq HBoxElem
singletonHBoxElemSeq = pure

fmtHBoxElemSeq :: Fmt (Seq HBoxElem)
fmtHBoxElemSeq = F.intercalated "\n" fmtHBoxElem

fmtHBoxElemSeqOneLine :: Fmt (Seq HBoxElem)
fmtHBoxElemSeqOneLine = fmtSeqOneLine fmtHBoxElem

hBoxNaturalDepth :: Seq HBoxElem -> Q.Length
hBoxNaturalDepth hBox =
  -- The empty HBox has zero depth.
  fromMaybe Q.zeroLength $
    maximumOf (traversed % to hBoxElemNaturalDepth) hBox

hBoxNaturalHeight :: Seq HBoxElem -> Q.Length
hBoxNaturalHeight hBox =
  -- The empty HBox has zero height.
  fromMaybe Q.zeroLength $
    maximumOf (traversed % to hBoxElemNaturalHeight) hBox

hBoxNaturalWidth :: Seq HBoxElem -> Q.Length
hBoxNaturalWidth =
  foldMapOf traversed hBoxElemNaturalWidth

fmtVBoxElemSeq :: Fmt (Seq VBoxElem)
fmtVBoxElemSeq = F.intercalated "\n" fmtVBoxElemOneLine
