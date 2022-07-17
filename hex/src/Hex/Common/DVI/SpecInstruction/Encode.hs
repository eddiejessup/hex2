module Hex.Common.DVI.SpecInstruction.Encode where

import Data.ByteString qualified as BS
import Data.Serialize (Put, put)
import Data.Serialize qualified as Ser
import Hex.Common.DVI.SpecInstruction.Decode
import Hex.Common.DVI.SpecInstruction.Types
import Hexlude hiding (put)

bLengthToWord :: ByteLength -> Word8
bLengthToWord = \case
  OneByte -> 0
  TwoByte -> 1
  FourByte -> 4

uOpOffset :: UnsignedNByteInt -> Word8
uOpOffset = bLengthToWord . uLength

sOpOffset :: SignedNByteInt -> Word8
sOpOffset = bLengthToWord . sLength

encodeSpecInstruction :: SpecInstruction -> ByteString
encodeSpecInstruction i =
  Ser.runPut $ specInstructionByteBuilder i

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
  BodySpecInstruction (BeginPageOp BeginPageOpArgs {numbers, lastBeginPagePointer}) -> do
    putOp 139 -- Op
    put numbers
    put lastBeginPagePointer
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
  BodySpecInstruction (DefineFontOp args) -> do
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
    put args.uselessWord
    put args.uselessString
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
    put args.signatureBytes
  where
    putAmbleArgs :: AmbleArgs -> Put
    putAmbleArgs ambleArgs = do
      put ambleArgs.numerator
      put ambleArgs.denominator
      put ambleArgs.magnification
