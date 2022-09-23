{-# LANGUAGE UndecidableInstances #-}

module Hex.Common.HexIO.Impl where

import Data.Sequence qualified as Seq
import Effectful.FileSystem qualified as FS
import Formatting qualified as F
import Hex.Capability.Log.Interface (HexLog)
import Hex.Capability.Log.Interface qualified as Log
import Hex.Common.Codes qualified as Code
import Hex.Common.HexEnv.Interface qualified as HEnv
import Hex.Common.HexIO.Impl.CharSource qualified as CharSource
import Hex.Common.HexIO.Impl.CharSourceStack (CharSourceStack)
import Hex.Common.HexIO.Impl.CharSourceStack qualified as CharSourceStack
import Hex.Common.HexIO.Impl.IOState (IOState)
import Hex.Common.HexIO.Impl.IOState qualified as IOState
import Hex.Common.HexIO.Impl.Lex qualified as HIO
import Hex.Common.HexIO.Interface (HexIO (..))
import Hex.Common.HexIO.Interface qualified as HIO
import Hex.Common.HexState.Interface (EHexState)
import Hex.Common.HexState.Interface qualified as HSt
import Hex.Common.HexState.Interface.Parameter qualified as HSt.Param
import Hex.Common.Quantity qualified as Q
import Hex.Common.Token.Lexed qualified as LT
import Hexlude

runHexIO ::
  [Error HIO.LexError, State IOState, HEnv.EHexEnv, HexLog, EHexState, FS.FileSystem] :>> es =>
  Eff (HexIO : es) a ->
  Eff es a
runHexIO = interpret $ \_ -> \case
  EndCurrentLine -> endCurrentLineImpl
  InputIsFinished -> inputIsFinishedImpl
  GetInput -> getInputImpl
  PutInput charSourceStack -> putInputImpl charSourceStack
  InsertLexToken lexToken -> insertLexTokenImpl lexToken
  InsertLexTokens lexTokens -> insertLexTokensImpl lexTokens
  GetNextLexToken -> getNextLexTokenImpl
  ReadTexFile hexFilePath -> readTexFileImpl hexFilePath
  OpenStreamFile hexFilePath streamNr inOrOut -> openInputFileImpl hexFilePath streamNr inOrOut

putInputImpl :: State IOState :> es => CharSourceStack -> Eff es ()
putInputImpl charSourceStack = assign @IOState #sourceStack charSourceStack

getInputImpl :: State IOState :> es => Eff es CharSourceStack
getInputImpl = use @IOState #sourceStack

getEndLineCharCode :: EHexState :> es => Eff es (Maybe Code.CharCode)
getEndLineCharCode =
  Code.fromHexInt <$> HSt.getParameterValue (HSt.Param.IntQuantParam HSt.Param.EndLineChar)

endCurrentLineImpl ::
  [State IOState, EHexState, Error HIO.LexError] :>> es =>
  Eff es ()
endCurrentLineImpl = do
  stack <- getInputImpl
  -- End the line, update the char-source to the new state
  CharSource.endSourceCurrentLine <$> getEndLineCharCode <*> pure (CharSourceStack.currentSource stack) >>= \case
    Just newCharSource ->
      assign @IOState (#sourceStack % CharSourceStack.currentSourceLens) newCharSource
    Nothing -> do
      case CharSourceStack.endCurrentCharSource stack of
        Nothing ->
          throwError HIO.NoMoreLines
        Just newStack -> do
          putInputImpl newStack

inputIsFinishedImpl :: State IOState :> es => Eff es Bool
inputIsFinishedImpl = use @IOState (#sourceStack % to CharSourceStack.stackIsFinished)

-- Insert in reverse order, so, we insert the "r" of "relax" last, so we pop "r" next.
insertLexTokensImpl :: State IOState :> es => Seq LT.LexToken -> Eff es ()
insertLexTokensImpl lts = forM_ (Seq.reverse lts) insertLexTokenImpl

insertLexTokenImpl :: State IOState :> es => LT.LexToken -> Eff es ()
insertLexTokenImpl lt =
  modifying
    @IOState
    (#sourceStack % CharSourceStack.currentSourceLens)
    (CharSource.insertLexTokenToSource lt)

extractNextLexTokenFromWorkingLineBuffer :: State IOState :> es => Eff es (Maybe LT.LexToken)
extractNextLexTokenFromWorkingLineBuffer = do
  use @IOState (#sourceStack % CharSourceStack.currentSourceLens % #workingLine % #workingLexTokens) >>= \case
    lt :<| ltRest -> do
      assign @IOState (#sourceStack % CharSourceStack.currentSourceLens % #workingLine % #workingLexTokens) ltRest
      pure $ Just lt
    Empty ->
      pure Nothing

extractNextLexTokenFromWorkingLineSource ::
  [EHexState, State IOState, Error HIO.LexError, HexLog] :>> es =>
  Eff es (Maybe LT.LexToken)
extractNextLexTokenFromWorkingLineSource = do
  lineState <- use @IOState (#sourceStack % CharSourceStack.currentSourceLens % #workingLine % #sourceLine % #lineState)
  HIO.extractLexTokenFromSourceLine lineState >>= \case
    Nothing -> pure Nothing
    Just (lt, newLineState) -> do
      -- Logging.
      lineNr <- use @IOState (#sourceStack % CharSourceStack.currentSourceLens % #lineNr)
      let logFmtString = CharSource.fmtLineNr |%| ": Fetched lex-token from source: " |%| LT.fmtLexToken
      Log.infoLog $
        if newLineState == lineState
          then F.sformat logFmtString lineNr lt
          else F.sformat (logFmtString |%| ", " |%| CharSource.fmtLineState |%| " -> " |%| CharSource.fmtLineState) lineNr lt lineState newLineState
      -- Back to logic.
      assign @IOState (#sourceStack % CharSourceStack.currentSourceLens % #workingLine % #sourceLine % #lineState) newLineState
      pure $ Just lt

getNextLexTokenImpl ::
  [EHexState, State IOState, Error HIO.LexError, HexLog] :>> es =>
  Eff es (Maybe LT.LexToken)
getNextLexTokenImpl = do
  extractNextLexTokenFromWorkingLineBuffer >>= \case
    Just lt ->
      pure $ Just lt
    Nothing ->
      extractNextLexTokenFromWorkingLineSource >>= \case
        Just lt -> pure $ Just lt
        Nothing ->
          inputIsFinishedImpl >>= \case
            True -> pure Nothing
            False -> do
              endCurrentLineImpl
              getNextLexTokenImpl

readTexFileImpl ::
  [EHexState, FS.FileSystem, HEnv.EHexEnv, State IOState, Error HIO.LexError] :>> es =>
  HexFilePath ->
  Eff es ()
readTexFileImpl inputPath = do
  inputBytes <-
    HEnv.findAndReadFile
      (HEnv.WithImplicitExtension "tex")
      inputPath.unHexFilePath
      >>= note (HIO.FileNotFound inputPath)
  endLineCode <- getEndLineCharCode
  modifying @IOState #sourceStack (CharSourceStack.pushCharSource endLineCode inputBytes)

openInputFileImpl ::
  [EHexState, State IOState, HEnv.EHexEnv, Error HIO.LexError, FS.FileSystem] :>> es =>
  HexFilePath ->
  Q.FourBitInt ->
  HIO.InputOrOutput ->
  Eff es ()
openInputFileImpl inputPath streamNr inOrOut = do
  fileHandle <-
    HEnv.findAndOpenFile
      HEnv.NoImplicitExtension
      inputPath.unHexFilePath
      (IOState.toIOMode inOrOut)
      >>= note (HIO.FileNotFound inputPath)
  assign @IOState (IOState.streamsLens inOrOut % ix streamNr.unFourBitInt.unHexInt) (Just fileHandle)
