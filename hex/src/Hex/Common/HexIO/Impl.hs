{-# LANGUAGE UndecidableInstances #-}

module Hex.Common.HexIO.Impl where

import Data.Sequence qualified as Seq
import Effectful.FileSystem qualified as FS
import Effectful.FileSystem.IO qualified as FS.IO
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
import System.FilePath qualified as FilePath

runHexIO ::
  (Error HIO.LexError :> es, State IOState :> es, HEnv.EHexEnv :> es, HexLog :> es, EHexState :> es, FS.FileSystem :> es) =>
  Eff (HexIO : es) a ->
  Eff es a
runHexIO = interpret $ \_ -> \case
  EndCurrentLine -> endCurrentLineImpl
  EndCurrentInput -> endCurrentInputImpl
  InputIsFinished -> inputIsFinishedImpl
  GetInput -> getInputImpl
  PutInput charSourceStack -> putInputImpl charSourceStack
  InsertLexToken lexToken -> insertLexTokenImpl lexToken
  InsertLexTokens lexTokens -> insertLexTokensImpl lexTokens
  GetNextLexToken -> getNextLexTokenImpl
  ReadTexFile hexFilePath -> readTexFileImpl hexFilePath
  OpenStreamFile hexFilePath streamNr inOrOut -> openStreamFileImpl hexFilePath streamNr inOrOut
  AtEndOfInputStreamFile streamNr -> atEndOfInputStreamFileImpl streamNr

putInputImpl :: (State IOState :> es) => CharSourceStack -> Eff es ()
putInputImpl charSourceStack = do
  assign @IOState #sourceStack charSourceStack

getInputImpl :: State IOState :> es => Eff es CharSourceStack
getInputImpl = use @IOState #sourceStack

getEndLineCharCode :: EHexState :> es => Eff es (Maybe Code.CharCode)
getEndLineCharCode =
  Code.fromHexInt <$> HSt.getParameterValue (HSt.Param.IntQuantParam HSt.Param.EndLineChar)

endCurrentLineImpl ::
  (State IOState :> es, EHexState :> es, Error HIO.LexError :> es) =>
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

endCurrentInputImpl ::
  (State IOState :> es) =>
  Eff es ()
endCurrentInputImpl = do
  modifying
    @IOState
    (#sourceStack % CharSourceStack.currentSourceLens)
    CharSource.endCharSourceAfterCurrentLine

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

extractNextLexTokenFromWorkingLineBuffer :: (State IOState :> es) => Eff es (Maybe LT.LexToken)
extractNextLexTokenFromWorkingLineBuffer = do
  use @IOState (#sourceStack % CharSourceStack.currentSourceLens % #workingLine % #workingLexTokens) >>= \case
    lt :<| ltRest -> do
      assign @IOState (#sourceStack % CharSourceStack.currentSourceLens % #workingLine % #workingLexTokens) ltRest
      pure $ Just lt
    Empty ->
      pure Nothing

extractNextLexTokenFromWorkingLineSource ::
  (EHexState :> es, State IOState :> es, Error HIO.LexError :> es, HexLog :> es) =>
  Eff es (Maybe LT.LexToken)
extractNextLexTokenFromWorkingLineSource = do
  lineState <- use @IOState (#sourceStack % CharSourceStack.currentSourceLens % #workingLine % #sourceLine % #lineState)
  HIO.extractLexTokenFromSourceLine lineState >>= \case
    Nothing -> pure Nothing
    Just (lt, newLineState) -> do
      -- Logging.
      let logFmtString = "Fetched lex-token from source: " |%| LT.fmtLexToken
      Log.debugLog $
        if newLineState == lineState
          then F.sformat logFmtString lt
          else F.sformat (logFmtString |%| ", " |%| CharSource.fmtLineState |%| " -> " |%| CharSource.fmtLineState) lt lineState newLineState
      -- Back to logic.
      assign @IOState (#sourceStack % CharSourceStack.currentSourceLens % #workingLine % #sourceLine % #lineState) newLineState
      pure $ Just lt

getNextLexTokenImpl ::
  (EHexState :> es, State IOState :> es, Error HIO.LexError :> es, HexLog :> es) =>
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
  (EHexState :> es, HEnv.EHexEnv :> es, State IOState :> es, Error HIO.LexError :> es) =>
  HexFilePath ->
  Eff es ()
readTexFileImpl inputPath = do
  inputBytes <-
    HEnv.findAndReadFile
      (HEnv.WithImplicitExtension "tex")
      inputPath
      >>= note (HIO.FileNotFound inputPath)
  endLineCode <- getEndLineCharCode
  let name = toS $ FilePath.takeBaseName inputPath.unHexFilePath
  modifying @IOState #sourceStack (CharSourceStack.pushCharSource name endLineCode inputBytes)

openStreamFileImpl ::
  (State IOState :> es, HEnv.EHexEnv :> es, FS.FileSystem :> es) =>
  HexFilePath ->
  Q.FourBitInt ->
  HIO.InputOrOutput ->
  Eff es ()
openStreamFileImpl inputPath streamNr inOrOut = do
  mayHandle <- case inOrOut of
    HIO.InputFile ->
      HEnv.findAndOpenFile HEnv.NoImplicitExtension inputPath FS.IO.ReadMode
    HIO.OutputFile ->
      Just <$> FS.IO.openFile inputPath.unHexFilePath FS.IO.WriteMode
  assign @IOState (IOState.streamLens inOrOut streamNr) mayHandle

atEndOfInputStreamFileImpl ::
  (State IOState :> es, FS.FileSystem :> es) =>
  Q.FourBitInt ->
  Eff es Bool
atEndOfInputStreamFileImpl streamNr = do
  use @IOState (IOState.streamLens HIO.InputFile streamNr) >>= \case
    Nothing -> pure True
    Just streamHandle -> FS.IO.hIsEOF streamHandle
