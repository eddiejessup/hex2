module Hex.Stage.Render.Interface.SpecInstruction where

import Formatting qualified as F
import Hex.Stage.Render.Interface.SpecInstruction.Decode qualified as Dec
import Hexlude

newtype Magnification a = Magnification {unMagnification :: a}
  deriving stock (Show, Generic)

data DVIError
  = OutOfBoundsValue Text Int
  deriving stock (Show, Generic)

fmtDVIError :: Fmt DVIError
fmtDVIError = F.shown

noteOutOfBounds :: (Error DVIError :> es) => Text -> (Int -> Maybe w) -> Int -> Eff es w
noteOutOfBounds ctx f n = case f n of
  Nothing -> throwError $ OutOfBoundsValue ctx n
  Just v -> pure v

data SpecInstruction
  = BodySpecInstruction BodySpecInstruction
  | DefineFontOp DefineFontOpArgs
  | PreambleOp PreambleOpArgs
  | PostambleOp PostambleOpArgs
  | PostPostambleOp PostPostambleOpArgs
  deriving stock (Show, Generic)

fmtSpecInstruction :: Fmt SpecInstruction
fmtSpecInstruction = F.later $ \case
  BodySpecInstruction bodySpecInstruction -> F.bformat fmtBodySpecInstruction bodySpecInstruction
  DefineFontOp defineFontOpArgs -> F.bformat F.text $ "Define Font: " <> show defineFontOpArgs
  PreambleOp preambleOpArgs -> F.bformat F.text $ "Preamble: " <> show preambleOpArgs
  PostambleOp postambleOpArgs -> F.bformat F.text $ "Postamble: " <> show postambleOpArgs
  PostPostambleOp postPostambleOpArgs -> F.bformat F.text $ "PostPostamble: " <> show postPostambleOpArgs

fmtSpecInstructions :: Fmt [SpecInstruction]
fmtSpecInstructions = F.unlined fmtSpecInstruction

data BodySpecInstruction
  = AddCharOp CharOpArgs
  | AddRuleOp AddRuleOpArgs
  | BeginPageOp BeginPageOpArgs
  | EndPageOp
  | PushOp
  | PopOp
  | MoveOp Axis Dec.SignedNByteInt
  | SelectFontOp SelectFontOpArgs
  | DoSpecialOp Dec.ByteLength
  deriving stock (Show, Generic)

fmtBodySpecInstruction :: Fmt BodySpecInstruction
fmtBodySpecInstruction = F.later $ \case
  AddCharOp charOpArgs -> F.bformat F.text $ "Add char: " <> show charOpArgs
  AddRuleOp addRuleOpArgs -> F.bformat F.text $ "Add rule: " <> show addRuleOpArgs
  BeginPageOp beginPageOpArgs -> F.bformat F.text $ "Begin Page: " <> show beginPageOpArgs
  EndPageOp -> F.bformat F.text $ "End Page"
  PushOp -> F.bformat F.text $ "Push"
  PopOp -> F.bformat F.text $ "Pop"
  MoveOp Vertical signedNByteInt -> F.bformat F.text $ "Move down: " <> show signedNByteInt
  MoveOp Horizontal signedNByteInt -> F.bformat F.text $ "Move right: " <> show signedNByteInt
  SelectFontOp selectFontOpArgs -> F.bformat F.text $ "Select Font: " <> show selectFontOpArgs
  DoSpecialOp byteLength -> F.bformat F.text $ "Do Special: " <> show byteLength

data AddRuleOpArgs = AddRuleOpArgs
  { moveMode :: MoveMode,
    vSpan :: Word32,
    hSpan :: Word32
  }
  deriving stock (Show, Generic)

data CharOpArgs = FastSetCharOp Word8 | ArgCharOp MoveMode Dec.UnsignedNByteInt
  deriving stock (Show, Generic)

data SelectFontOpArgs = FastSelectFontOp Word8 | ArgSelectFontOp Dec.UnsignedNByteInt
  deriving stock (Show, Generic)

data BeginPageOpArgs = BeginPageOpArgs
  { count0 :: Int32,
    count1 :: Int32,
    count2 :: Int32,
    count3 :: Int32,
    count4 :: Int32,
    count5 :: Int32,
    count6 :: Int32,
    count7 :: Int32,
    count8 :: Int32,
    count9 :: Int32,
    lastBeginPagePointer :: Int32
  }
  deriving stock (Show, Generic)

data DefineFontOpArgs = DefineFontOpArgs
  { fontNr :: Dec.UnsignedNByteInt,
    checksum :: Word32,
    scaleFactor :: Int32,
    designSize :: Int32,
    dirPathLength :: Word8,
    fileNameLength :: Word8,
    fontPath :: Dec.SpecByteString
  }
  deriving stock (Show, Generic)

data AmbleArgs = AmbleArgs
  { numerator :: Int32,
    denominator :: Int32,
    magnification :: Int32
  }
  deriving stock (Show, Generic)

data PreambleOpArgs = PreambleOpArgs
  { dviFormatPre :: Word8,
    ambleArgsPre :: AmbleArgs,
    commentLength :: Word8,
    comment :: Dec.SpecByteString
  }
  deriving stock (Show, Generic)

data PostambleOpArgs = PostambleOpArgs
  { lastPointer :: Int32,
    ambleArgsPost :: AmbleArgs,
    maxPageVSpan :: Int32,
    maxPageHSpan :: Int32,
    maxStackDepth :: Word16,
    numberOfPages :: Word16
  }
  deriving stock (Show, Generic)

data PostPostambleOpArgs = PostPostambleOpArgs
  { postamblePointer :: Int32,
    dviFormatPost :: Word8,
    signatureByte1 :: Word8,
    signatureByte2 :: Word8,
    signatureByte3 :: Word8,
    signatureByte4 :: Word8
  }
  deriving stock (Show, Generic)

data MoveMode
  = DoNoMove
  | DoAndMove
  deriving stock (Show)
