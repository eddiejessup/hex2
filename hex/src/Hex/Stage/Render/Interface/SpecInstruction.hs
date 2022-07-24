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
  | PreambleOp PreambleOpArgs
  | PostambleOp PostambleOpArgs
  | PostPostambleOp PostPostambleOpArgs
  deriving stock (Show, Generic)

fmtSpecInstruction :: Fmt SpecInstruction
fmtSpecInstruction = F.shown

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
  | -- | DoSpecial ByteLength
    DefineFontOp DefineFontOpArgs
  deriving stock (Show, Generic)

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
  { numbers :: [Int32],
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
    fontPath :: ByteString
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
    uselessWord :: Word8,
    uselessString :: ByteString
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
    signatureBytes :: [Word32]
  }
  deriving stock (Show, Generic)

data MoveMode
  = DoNoMove
  | DoAndMove
  deriving stock (Show)
