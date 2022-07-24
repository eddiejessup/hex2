{-# LANGUAGE UndecidableInstances #-}

module Hex.Stage.Read.Impl where

import Data.Sequence qualified as Seq
import Formatting qualified as F
import Hex.Capability.Log.Interface (HexLog)
import Hex.Capability.Log.Interface qualified as Log
import Hex.Common.Codes qualified as Code
import Hex.Common.HexEnv.Interface (EHexEnv)
import Hex.Common.HexEnv.Interface qualified as Env
import Hex.Common.HexState.Interface (EHexState)
import Hex.Common.HexState.Interface qualified as HSt
import Hex.Common.HexState.Interface.Parameter qualified as HSt.Param
import Hex.Common.Token.Lexed qualified as LT
import Hex.Stage.Read.Impl.CharSource qualified as CharSource
import Hex.Stage.Read.Impl.CharSourceStack (CharSourceStack)
import Hex.Stage.Read.Impl.CharSourceStack qualified as CharSourceStack
import Hex.Stage.Read.Impl.Lex qualified as HIn
import Hex.Stage.Read.Interface (HexInput (..))
import Hex.Stage.Read.Interface qualified as HIn
import Hexlude

runHexInput :: ([IOE, EHexEnv, Error HIn.LexError, State CharSourceStack, HexLog, EHexState] :>> es) => Eff (HexInput : es) a -> Eff es a
runHexInput = interpret $ \_ -> \case
  EndCurrentLine -> endCurrentLineImpl
  InputIsFinished -> inputIsFinishedImpl
  GetInput -> get @CharSourceStack
  PutInput charSourceStack -> put @CharSourceStack charSourceStack
  InsertLexToken lexToken -> insertLexTokenImpl lexToken
  InsertLexTokens lexTokens -> insertLexTokensImpl lexTokens
  GetNextLexToken -> getNextLexTokenImpl
  OpenInputFile hexFilePath -> openInputFileImpl hexFilePath

getEndLineCharCode :: EHexState :> es => Eff es (Maybe Code.CharCode)
getEndLineCharCode =
  Code.fromHexInt <$> HSt.getParameterValue (HSt.Param.IntQuantParam HSt.Param.EndLineChar)

endCurrentLineImpl ::
  [State CharSourceStack, EHexState, Error HIn.LexError] :>> es =>
  Eff es ()
endCurrentLineImpl = do
  stack <- get @CharSourceStack
  -- End the line, update the char-source to the new state
  CharSource.endSourceCurrentLine <$> getEndLineCharCode <*> pure (CharSourceStack.currentSource stack) >>= \case
    Just newCharSource ->
      assign @CharSourceStack CharSourceStack.currentSourceLens newCharSource
    Nothing -> do
      case CharSourceStack.endCurrentCharSource stack of
        Nothing ->
          throwError HIn.NoMoreLines
        Just newStack -> do
          put @CharSourceStack newStack

inputIsFinishedImpl :: State CharSourceStack :> es => Eff es Bool
inputIsFinishedImpl = get @CharSourceStack <&> CharSourceStack.stackIsFinished

-- Insert in reverse order, so, we insert the "r" of "relax" last, so we pop "r" next.
insertLexTokensImpl :: State CharSourceStack :> es => Seq LT.LexToken -> Eff es ()
insertLexTokensImpl lts = forM_ (Seq.reverse lts) insertLexTokenImpl

insertLexTokenImpl :: State CharSourceStack :> es => LT.LexToken -> Eff es ()
insertLexTokenImpl lt =
  modifying
    CharSourceStack.currentSourceLens
    (CharSource.insertLexTokenToSource lt)

extractNextLexTokenFromWorkingLineBuffer :: State CharSourceStack :> es => Eff es (Maybe LT.LexToken)
extractNextLexTokenFromWorkingLineBuffer = do
  use (CharSourceStack.currentSourceLens % #workingLine % #workingLexTokens) >>= \case
    lt :<| ltRest -> do
      assign @CharSourceStack (CharSourceStack.currentSourceLens % #workingLine % #workingLexTokens) ltRest
      pure $ Just lt
    Empty ->
      pure Nothing

extractNextLexTokenFromWorkingLineSource ::
  [EHexState, State CharSourceStack, Error HIn.LexError, HexLog] :>> es =>
  Eff es (Maybe LT.LexToken)
extractNextLexTokenFromWorkingLineSource = do
  lineState <- use (CharSourceStack.currentSourceLens % #workingLine % #sourceLine % #lineState)
  HIn.extractLexTokenFromSourceLine lineState >>= \case
    Nothing -> pure Nothing
    Just (lt, newLineState) -> do
      -- Logging.
      lineNr <- use (CharSourceStack.currentSourceLens % #lineNr)
      let logFmtString = CharSource.fmtLineNr |%| ": Fetched lex-token from source: " |%| LT.fmtLexToken
      Log.infoLog $
        if newLineState == lineState
          then F.sformat logFmtString lineNr lt
          else F.sformat (logFmtString |%| ", " |%| CharSource.fmtLineState |%| " -> " |%| CharSource.fmtLineState) lineNr lt lineState newLineState
      -- Back to logic.
      assign @CharSourceStack (CharSourceStack.currentSourceLens % #workingLine % #sourceLine % #lineState) newLineState
      pure $ Just lt

getNextLexTokenImpl ::
  [EHexState, State CharSourceStack, Error HIn.LexError, HexLog] :>> es =>
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

openInputFileImpl ::
  [EHexState, State CharSourceStack, EHexEnv, Error HIn.LexError] :>> es =>
  HexFilePath ->
  Eff es ()
openInputFileImpl inputPath = do
  inputBytes <-
    Env.findAndReadFile
      (Env.WithImplicitExtension "tex")
      (inputPath ^. typed @FilePath)
      >>= note (HIn.InputFileNotFound inputPath)
  endLineCode <- getEndLineCharCode
  modify $ CharSourceStack.pushCharSource endLineCode inputBytes
