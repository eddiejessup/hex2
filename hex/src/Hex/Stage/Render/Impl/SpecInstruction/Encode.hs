module Hex.Stage.Render.Impl.SpecInstruction.Encode where

import Data.ByteString qualified as BS
import Data.Serialize (Put, put)
import Data.Serialize qualified as Ser
import GHC.Num
import Hex.Stage.Render.Interface.SpecInstruction
import Hex.Stage.Render.Interface.SpecInstruction.Decode
import Hexlude hiding (put)

bLengthToWord :: ByteLength -> Word8
bLengthToWord = \case
  OneByte -> 0
  TwoByte -> 1
  FourByte -> 3

uOpOffset :: UnsignedNByteInt -> Word8
uOpOffset = bLengthToWord . uLength

sOpOffset :: SignedNByteInt -> Word8
sOpOffset = bLengthToWord . sLength

encodeSpecInstruction :: SpecInstruction -> ByteString
encodeSpecInstruction i =
  Ser.runPut $ specInstructionByteBuilder i

-- >>> BS.unpack $ Ser.runPut (put (mempty :: ByteString))
-- [0,0,0,0,0,0,0,0]
-- >>> BS.unpack $ Ser.runPut (put ([] :: [Word8]))
-- [0,0,0,0,0,0,0,0]
-- >>> BS.unpack $ Ser.runPut (Ser.putListOf Ser.putWord8 ([] :: [Word8]))
-- [0,0,0,0,0,0,0,0]
-- >>> BS.unpack $ Ser.runPut (put (SpecByteString mempty))
-- []
-- >>> BS.unpack $ Ser.runPut (put (1 :: Int32))
-- [0,0,0,1]
-- >>> BS.unpack $ Ser.runPut (put (-1 :: Int32))
-- [255,255,255,255]

encodeSpecInstructions :: [SpecInstruction] -> ByteString
encodeSpecInstructions is =
  Ser.runPut $ for_ is specInstructionByteBuilder

putOp :: Word8 -> Put
putOp = put

specInstructionLength :: SpecInstruction -> Int
specInstructionLength i = BS.length (encodeSpecInstruction i)

specInstructionByteBuilder :: SpecInstruction -> Put
specInstructionByteBuilder = \case
  BodySpecInstruction (AddCharOp (FastSetCharOp w8)) ->
    putOp w8 -- Op
  BodySpecInstruction (AddCharOp (ArgCharOp moveMode un)) -> do
    let baseOp = case moveMode of
          DoAndMove -> 128
          DoNoMove -> 133
    putOp $ baseOp + uOpOffset un -- Op
    put un
  BodySpecInstruction (AddRuleOp AddRuleOpArgs {moveMode, vSpan, hSpan}) -> do
    putOp $ case moveMode of -- Op
      DoAndMove -> 132
      DoNoMove -> 137
    put hSpan
    put vSpan
  BodySpecInstruction (BeginPageOp beginPageOpArgs) -> do
    putOp 139 -- Op
    put beginPageOpArgs.count0
    put beginPageOpArgs.count1
    put beginPageOpArgs.count2
    put beginPageOpArgs.count3
    put beginPageOpArgs.count4
    put beginPageOpArgs.count5
    put beginPageOpArgs.count6
    put beginPageOpArgs.count7
    put beginPageOpArgs.count8
    put beginPageOpArgs.count9
    put beginPageOpArgs.lastBeginPagePointer
  BodySpecInstruction EndPageOp ->
    putOp 140 -- Op
  BodySpecInstruction PushOp ->
    putOp 141 -- Op
  BodySpecInstruction PopOp ->
    putOp 142 -- Op
  BodySpecInstruction (MoveOp Horizontal sn) -> do
    putOp $ 143 + sOpOffset sn -- Op
    put sn
  BodySpecInstruction (MoveOp Vertical sn) -> do
    putOp $ 157 + sOpOffset sn -- Op
    put sn
  BodySpecInstruction (SelectFontOp (FastSelectFontOp w8)) ->
    putOp $ 171 + w8 -- Op
  BodySpecInstruction (SelectFontOp (ArgSelectFontOp un)) -> do
    putOp $ 235 + uOpOffset un -- Op
    put un
  BodySpecInstruction (DoSpecialOp _byteLength) ->
    notImplemented "encode DoSpecialOp"
  DefineFontOp args -> do
    putOp $ 243 + uOpOffset args.fontNr
    -- Op
    put args.fontNr
    put args.checksum
    put args.scaleFactor
    put args.designSize
    put args.dirPathLength
    put args.fileNameLength
    put args.fontPath
  PreambleOp args -> do
    putOp 247
    put args.dviFormatPre
    putAmbleArgs args.ambleArgsPre
    put args.commentLength
    put args.comment
  PostambleOp args -> do
    putOp 248
    put args.lastPointer
    putAmbleArgs args.ambleArgsPost
    put args.maxPageVSpan
    put args.maxPageHSpan
    put args.maxStackDepth
    put args.numberOfPages
  PostPostambleOp args -> do
    putOp 249
    put args.postamblePointer
    put args.dviFormatPost
    put args.signatureByte1
    put args.signatureByte2
    put args.signatureByte3
    put args.signatureByte4
  where
    putAmbleArgs :: AmbleArgs -> Put
    putAmbleArgs ambleArgs = do
      put ambleArgs.numerator
      put ambleArgs.denominator
      put ambleArgs.magnification
