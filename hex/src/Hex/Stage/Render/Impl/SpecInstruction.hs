module Hex.Stage.Render.Impl.SpecInstruction where

import Data.ByteString qualified as BS
import Data.ByteString.Char8 qualified as BS.Char8
import Data.List qualified as List
import Hex.Common.Codes qualified as Codes
import Hex.Common.Quantity qualified as Q
import Hex.Stage.Render.Impl.SpecInstruction.Encode qualified as Enc
import Hex.Stage.Render.Interface.DocInstruction (DocInstruction (..))
import Hex.Stage.Render.Interface.DocInstruction qualified as DocInstruction
import Hex.Stage.Render.Interface.SpecInstruction
import Hex.Stage.Render.Interface.SpecInstruction.Decode qualified as Dec
import Hexlude
import System.FilePath qualified as FilePath

newtype BytePointer = BytePointer {unBytePointer :: Int32}
  deriving stock (Show, Generic)
  deriving (Semigroup, Monoid, Group) via (Sum Int32)

docAsBodySpecInstruction :: (Error DVIError :> es) => BytePointer -> DocInstruction -> Eff es BodySpecInstruction
docAsBodySpecInstruction lastBeginPagePointer = \case
  BeginNewPage ->
    pure $
      BeginPageOp
        BeginPageOpArgs
          { numbers = [0 .. 9],
            lastBeginPagePointer = lastBeginPagePointer.unBytePointer
          }
  EndPage ->
    pure EndPageOp
  DefineFont fontDefinition -> do
    let (dirPathRaw, fileNameRaw) = FilePath.splitFileName (fontDefinition.fontPath.unHexFilePath)
        dirPath = BS.Char8.pack $ stripLeadingDot dirPathRaw
        fileName = BS.Char8.pack fileNameRaw

    dirPathLength <- noteOutOfBounds "dirPathLength" Dec.intToWord8 (BS.length dirPath)
    fileNameLength <- noteOutOfBounds "fileNameLength" Dec.intToWord8 (BS.length fileName)
    fontNrSized <- noteOutOfBounds "fontNrSized" Dec.intToUnsigned fontDefinition.fontNr.unFontNumber.unHexInt
    scaleFactorSized <- noteOutOfBounds "scaleFactorSized" Dec.intToInt32 (fontDefinition.fontDefDesignScale.unLength.unHexInt)
    designSizeSized <- noteOutOfBounds "designSizeSized" Dec.intToInt32 (fontDefinition.fontDefDesignSize.unLength.unHexInt)

    pure $
      DefineFontOp
        DefineFontOpArgs
          { fontNr = fontNrSized,
            checksum = fontDefinition.fontDefChecksum,
            scaleFactor = scaleFactorSized,
            designSize = designSizeSized,
            dirPathLength,
            fileNameLength,
            fontPath = dirPath <> fileName
          }
  SelectFont fontNumber -> do
    let fontNumberInt = fontNumber.unFontNumber.unHexInt
    arg <- case Dec.intToWord8 fontNumberInt of
      Just fontNumberW8
        | fontNumberW8 < 64 ->
            pure $ FastSelectFontOp fontNumberW8
      _ -> do
        fontNrSized <- noteOutOfBounds "fontNr" Dec.intToUnsigned fontNumberInt
        pure $ ArgSelectFontOp fontNrSized
    pure $ SelectFontOp arg
  AddCharacter charCode ->
    let charCodeW8 = charCode.unCharCode
     in pure $
          if charCodeW8 < 128
            then AddCharOp $ FastSetCharOp charCodeW8
            else AddCharOp $ ArgCharOp DoAndMove $ Dec.Unsigned1ByteInt charCodeW8
  AddRule ruleSpan -> do
    vSpanW32 <- noteOutOfBounds "Rule-vSpan" Dec.intToWord32 (ruleSpan.vSpan.unLength.unHexInt)
    hSpanW32 <- noteOutOfBounds "Rule-hSpan" Dec.intToWord32 (ruleSpan.hSpan.unLength.unHexInt)
    pure $
      AddRuleOp $
        AddRuleOpArgs
          { moveMode = DoAndMove,
            vSpan = vSpanW32,
            hSpan = hSpanW32
          }
  Move ax dist -> do
    sizedDistance <- noteOutOfBounds "Move-distance" Dec.intToSigned (dist.unLength.unHexInt)
    pure $ MoveOp ax sizedDistance
  PushStack ->
    pure PushOp
  PopStack ->
    pure PopOp
  where
    stripLeadingDot x =
      if x == "./"
        then ""
        else x

renderDocInstructions :: Magnification Q.HexInt -> [DocInstruction] -> (Maybe DVIError, SpecInstructionWriterState, [SpecInstruction])
renderDocInstructions mag docInstrs =
  let ranErr :: Eff [State SpecInstructionWriterState, Writer [SpecInstruction]] (Either DVIError ())
      ranErr = runErrorNoCallStack (addAllInstructions mag docInstrs)

      ranState :: Eff '[Writer [SpecInstruction]] (Either DVIError (), SpecInstructionWriterState)
      ranState = runStateLocal (SpecInstructionWriterState {currentBytePointer = BytePointer 0, beginPagePointers = [], curFontNr = Nothing, stackDepth = 0, maxStackDepth = 0}) ranErr

      ranWriter :: ((Either DVIError (), SpecInstructionWriterState), [SpecInstruction])
      ranWriter = runPureEff $ runWriterLocal ranState
   in ranWriter & \((errOrUnit, finalState), specInstrs) ->
        let mayErr = case errOrUnit of
              Left err -> Just err
              Right () -> Nothing
         in (mayErr, finalState, specInstrs)

data SpecInstructionWriterState = SpecInstructionWriterState
  { currentBytePointer :: BytePointer,
    beginPagePointers :: [BytePointer],
    curFontNr :: Maybe DocInstruction.FontNumber,
    stackDepth :: Word16,
    maxStackDepth :: Word16
  }
  deriving stock (Show, Generic)

ambleArgs :: Magnification Int32 -> AmbleArgs
ambleArgs mag =
  -- Define a fraction by which all dimensions should be multiplied to get
  -- lengths in units of 10^(-7) meters.
  AmbleArgs
    { numerator = 254 * (10 ^ (5 :: Int)),
      denominator = 7227 * (2 ^ (16 :: Int)),
      magnification = mag.unMagnification
    }

dviFormatWord :: Word8
dviFormatWord = 2

preambleOpArgs :: Magnification Int32 -> PreambleOpArgs
preambleOpArgs mag =
  PreambleOpArgs
    { dviFormatPre = dviFormatWord,
      ambleArgsPre = ambleArgs mag,
      uselessWord = 0,
      uselessString = ""
    }

getPostambleOpArgs :: ([Writer [SpecInstruction], State SpecInstructionWriterState, Error DVIError] :>> es) => Magnification Int32 -> Eff es PostambleOpArgs
getPostambleOpArgs mag = do
  lastPointer <- getPrevBeginPagePointer
  maxStackDepth <- use @SpecInstructionWriterState #maxStackDepth
  numberOfPages <- getNrBeginPagePointers
  pure
    PostambleOpArgs
      { lastPointer = lastPointer.unBytePointer,
        ambleArgsPost = ambleArgs mag,
        maxPageVSpan = 1,
        maxPageHSpan = 1,
        maxStackDepth,
        numberOfPages
      }

postPostambleArgs :: BytePointer -> PostPostambleOpArgs
postPostambleArgs postamblePointer =
  PostPostambleOpArgs
    { postamblePointer = postamblePointer.unBytePointer,
      dviFormatPost = dviFormatWord,
      signatureBytes = replicate 4 223
    }

emitSpecInstruction :: ([Writer [SpecInstruction], State SpecInstructionWriterState, Error DVIError] :>> es) => SpecInstruction -> Eff es ()
emitSpecInstruction specI = do
  -- Compute the number of bytes in the spec-instruction.

  nBytesInt32 <- noteOutOfBounds "Instruction length" Dec.intToInt32 (Enc.specInstructionLength specI)
  -- Emit the spec-instruction.
  tell @[SpecInstruction] [specI]
  -- Update the current byte-pointer to reflect the instruction.
  modifying @SpecInstructionWriterState #currentBytePointer (<> (BytePointer nBytesInt32))

getPrevBeginPagePointer :: (State SpecInstructionWriterState :> es) => Eff es BytePointer
getPrevBeginPagePointer = use @SpecInstructionWriterState $ #beginPagePointers % to (fromMaybe (BytePointer (-1)) . lastMay)

getNrBeginPagePointers :: ([State SpecInstructionWriterState, Error DVIError] :>> es) => Eff es Word16
getNrBeginPagePointers = do
  nInt <- use @SpecInstructionWriterState (#beginPagePointers % to List.length)
  noteOutOfBounds "nrBeginPagePointers" Dec.intToWord16 nInt

emitDocInstruction :: ([Writer [SpecInstruction], State SpecInstructionWriterState, Error DVIError] :>> es) => DocInstruction -> Eff es ()
emitDocInstruction i = do
  -- Get the most recent begin-page pointer.
  prevBeginPagePointer <- getPrevBeginPagePointer
  -- Render the doc-instruction as a spec-instruction.
  specI <- BodySpecInstruction <$> docAsBodySpecInstruction prevBeginPagePointer i
  emitSpecInstruction specI

interpretDocInstruction :: ([Writer [SpecInstruction], State SpecInstructionWriterState, Error DVIError] :>> es) => DocInstruction -> Eff es ()
interpretDocInstruction i = do
  case i of
    BeginNewPage -> do
      -- Add a pointer for the new begin-page instruction.
      newBeginPageBytePointer <- use @SpecInstructionWriterState #currentBytePointer
      modifying @SpecInstructionWriterState (#beginPagePointers) (newBeginPageBytePointer :)
    PushStack -> do
      newStackDepth <- use @SpecInstructionWriterState (#stackDepth % to succ)
      assign @SpecInstructionWriterState #stackDepth newStackDepth
      modifying @SpecInstructionWriterState (#maxStackDepth) (max newStackDepth)
    SelectFont n -> do
      assign @SpecInstructionWriterState #curFontNr (Just n)
    _ ->
      pure ()

  emitDocInstruction i

  -- If a font is selected, add an instruction to select it again on the
  -- new page.
  case i of
    BeginNewPage -> do
      use @SpecInstructionWriterState #curFontNr >>= \case
        Nothing -> pure ()
        Just n -> do
          emitDocInstruction (SelectFont n)
    _ ->
      pure ()

addAllInstructions :: ([Writer [SpecInstruction], State SpecInstructionWriterState, Error DVIError] :>> es) => Magnification Q.HexInt -> [DocInstruction] -> Eff es ()
addAllInstructions mag docInstrs = do
  magInt32 <- Magnification <$> noteOutOfBounds "mag" Dec.intToInt32 (mag.unMagnification.unHexInt)

  -- Emit preamble instructions.
  emitSpecInstruction $ PreambleOp (preambleOpArgs magInt32)
  -- Emit instructions for all the 'doc' instructions.

  for_ docInstrs $ \docInstr ->
    interpretDocInstruction docInstr

  postamblePointer <- use @SpecInstructionWriterState #currentBytePointer
  -- Emit postamble instruction.
  postambleArgs <- getPostambleOpArgs magInt32
  emitSpecInstruction (PostambleOp postambleArgs)

  -- Re-emit the define-font instructions in the doc-instructions.
  for_ docInstrs $ \e -> case e of
    DefineFont _ ->
      emitDocInstruction e
    _ ->
      pure ()

  -- Emit the post-post-amble instruction.
  emitSpecInstruction $ PostPostambleOp (postPostambleArgs postamblePointer)
