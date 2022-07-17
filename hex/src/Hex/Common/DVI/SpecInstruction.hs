module Hex.Common.DVI.SpecInstruction where

import Control.Monad.Writer qualified as Writer
import Data.ByteString qualified as BS
import Data.ByteString.Char8 qualified as BS.Char8
import Data.List qualified as List
import Formatting qualified as F
import Hex.Common.Codes qualified as Codes
import Hex.Common.DVI.DocInstruction (DocInstruction (..))
import Hex.Common.DVI.DocInstruction qualified as DocInstruction
import Hex.Common.DVI.SpecInstruction.Decode qualified as Dec
import Hex.Common.DVI.SpecInstruction.Encode qualified as Enc
import Hex.Common.DVI.SpecInstruction.Types
import Hex.Common.Quantity qualified as Q
import Hexlude
import System.FilePath qualified as FilePath

data DVIError
  = OutOfBoundsValue Text Int
  deriving stock (Show, Generic)

fmtDVIError :: Fmt DVIError
fmtDVIError = F.shown

noteOutOfBounds :: (MonadError e m, AsType DVIError e) => Text -> (Int -> Maybe w) -> Int -> m w
noteOutOfBounds ctx f n = case f n of
  Nothing -> throwError $ injectTyped $ OutOfBoundsValue ctx n
  Just v -> pure v

docAsBodySpecInstruction :: (MonadError e m, AsType DVIError e) => BytePointer -> DocInstruction -> m BodySpecInstruction
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

renderDocInstructions :: AsType DVIError e => Magnification Q.HexInt -> [DocInstruction] -> (Maybe e, SpecInstructionWriterState, [SpecInstruction])
renderDocInstructions mag docInstrs =
  let ranErr = runExceptT $ unDocInstructionWriterT (addAllInstructions mag docInstrs)
      ranState = runStateT ranErr SpecInstructionWriterState {currentBytePointer = BytePointer 0, beginPagePointers = [], curFontNr = Nothing, stackDepth = 0, maxStackDepth = 0}
      ranWriter = Writer.runWriter ranState
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

newtype SpecInstructionWriterT e a = SpecInstructionWriterT {unDocInstructionWriterT :: ExceptT e (StateT SpecInstructionWriterState (Writer.Writer [SpecInstruction])) a}
  deriving stock (Generic)
  deriving newtype
    ( Functor,
      Applicative,
      Monad,
      Writer.MonadWriter [SpecInstruction],
      MonadState SpecInstructionWriterState,
      MonadError e
    )

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

getPostambleOpArgs :: (Writer.MonadWriter [SpecInstruction] m, MonadState SpecInstructionWriterState m, MonadError e m, AsType DVIError e) => Magnification Int32 -> m PostambleOpArgs
getPostambleOpArgs mag = do
  lastPointer <- getPrevBeginPagePointer
  maxStackDepth <- use #maxStackDepth
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

emitSpecInstruction :: (Writer.MonadWriter [SpecInstruction] m, MonadState SpecInstructionWriterState m, MonadError e m, AsType DVIError e) => SpecInstruction -> m ()
emitSpecInstruction specI = do
  -- Compute the number of bytes in the spec-instruction.

  nBytesInt32 <- noteOutOfBounds "Instruction length" Dec.intToInt32 (Enc.specInstructionLength specI)
  -- Emit the spec-instruction.
  Writer.tell [specI]
  -- Update the current byte-pointer to reflect the instruction.
  modifying' #currentBytePointer (<> (BytePointer nBytesInt32))

getPrevBeginPagePointer :: (MonadState SpecInstructionWriterState m) => m BytePointer
getPrevBeginPagePointer = use $ #beginPagePointers % to (fromMaybe (BytePointer (-1)) . lastMay)

getNrBeginPagePointers :: (MonadState SpecInstructionWriterState m, MonadError e m, AsType DVIError e) => m Word16
getNrBeginPagePointers = do
  nInt <- use (#beginPagePointers % to List.length)
  noteOutOfBounds "nrBeginPagePointers" Dec.intToWord16 nInt

emitDocInstruction :: (Writer.MonadWriter [SpecInstruction] m, MonadState SpecInstructionWriterState m, MonadError e m, AsType DVIError e) => DocInstruction -> m ()
emitDocInstruction i = do
  -- Get the most recent begin-page pointer.
  prevBeginPagePointer <- getPrevBeginPagePointer
  -- Render the doc-instruction as a spec-instruction.
  specI <- BodySpecInstruction <$> docAsBodySpecInstruction prevBeginPagePointer i
  emitSpecInstruction specI

interpretDocInstruction :: (Writer.MonadWriter [SpecInstruction] m, MonadState SpecInstructionWriterState m, MonadError e m, AsType DVIError e) => DocInstruction -> m ()
interpretDocInstruction i = do
  case i of
    BeginNewPage -> do
      -- Add a pointer for the new begin-page instruction.
      newBeginPageBytePointer <- use #currentBytePointer
      modifying' (#beginPagePointers) (newBeginPageBytePointer :)
    PushStack -> do
      newStackDepth <- use (#stackDepth % to succ)
      assign' #stackDepth newStackDepth
      modifying' (#maxStackDepth) (max newStackDepth)
    SelectFont n -> do
      assign' #curFontNr (Just n)
    _ ->
      pure ()

  emitDocInstruction i

  -- If a font is selected, add an instruction to select it again on the
  -- new page.
  case i of
    BeginNewPage -> do
      use #curFontNr >>= \case
        Nothing -> pure ()
        Just n -> do
          emitDocInstruction (SelectFont n)
    _ ->
      pure ()

newtype Magnification a = Magnification {unMagnification :: a}
  deriving stock (Show, Generic)

addAllInstructions :: (AsType DVIError e) => Magnification Q.HexInt -> [DocInstruction] -> SpecInstructionWriterT e ()
addAllInstructions mag docInstrs = do
  magInt32 <- Magnification <$> noteOutOfBounds "mag" Dec.intToInt32 (mag.unMagnification.unHexInt)

  -- Emit preamble instructions.
  emitSpecInstruction $ PreambleOp (preambleOpArgs magInt32)
  -- Emit instructions for all the 'doc' instructions.

  for_ docInstrs $ \docInstr ->
    interpretDocInstruction docInstr

  postamblePointer <- use #currentBytePointer
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
