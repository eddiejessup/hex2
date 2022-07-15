{-# LANGUAGE UndecidableInstances #-}

module Hex.Common.HexInput.Impl where

import Data.Sequence qualified as Seq
import Hex.Capability.Log.Interface (MonadHexLog)
import Hex.Common.Codes qualified as Code
import Hex.Common.HexInput.Interface (MonadHexInput (..))
import Hex.Common.HexInput.Interface qualified as HIn
import Hex.Common.HexInput.Impl.CharSource qualified as CharSource
import Hex.Common.HexState.Interface (MonadHexState)
import Hex.Common.HexState.Interface qualified as HSt
import Hex.Common.HexState.Interface.Parameter qualified as HSt.Param
import Hex.Common.HexInput.Impl.Lex qualified as HIn
import Hex.Common.Token.Lexed qualified as LT
import Hexlude
import qualified Hex.Common.HexEnv.Interface as Env
import qualified Data.ByteString as BS
import qualified Hex.Common.Quantity as Q
import Hex.Common.HexInput.Impl.CharSourceStack (CharSourceStack)
import qualified Hex.Common.HexInput.Impl.CharSourceStack as CharSourceStack
import qualified Hex.Common.HexEnv.Interface as HEnv

newtype HexInputT m a = HexInputT {unHexInputT :: m a}
  deriving newtype
    ( Functor,
      Applicative,
      Monad,
      MonadIO,
      MonadState st,
      MonadReader st,
      MonadError e,
      MonadHexState,
      MonadHexLog,
      HEnv.MonadHexEnv
    )

instance
  ( Monad m,
    MonadError e (HexInputT m),
    AsType HIn.LexError e,
    MonadState st (HexInputT m),
    HasType CharSourceStack st,
    Env.MonadHexEnv (HexInputT m),
    MonadIO (HexInputT m),
    MonadHexState (HexInputT m)
  ) =>
  MonadHexInput (HexInputT m)
  where
  endCurrentLine = endCurrentLineImpl

  inputIsFinished = inputIsFinishedImpl

  getInput = use (typed @CharSourceStack)

  putInput = assign' (typed @CharSourceStack)

  insertLexToken = insertLexTokenImpl

  insertLexTokens = insertLexTokensImpl

  getNextLexToken = getNextLexTokenImpl

  openInputFile = openInputFileImpl

getEndLineCharCode :: HSt.MonadHexState m => m (Maybe Code.CharCode)
getEndLineCharCode =
  Code.fromHexInt <$> HSt.getParameterValue (HSt.Param.IntQuantParam HSt.Param.EndLineChar)

endCurrentLineImpl ::
  ( MonadState st m,
    HasType CharSourceStack st,
    HSt.MonadHexState m,
    MonadError e m,
    AsType HIn.LexError e
  ) =>
  m ()
endCurrentLineImpl = do
  stack <- use (typed @CharSourceStack)
  -- End the line, update the char-source to the new state
  CharSource.endSourceCurrentLine <$> getEndLineCharCode <*> pure (CharSourceStack.currentSource stack) >>= \case
    Just newCharSource ->
      assign' (typed @CharSourceStack % CharSourceStack.currentSourceLens) newCharSource
    Nothing -> do
      case CharSourceStack.endCurrentCharSource stack of
        Nothing ->
          throwError $ injectTyped HIn.NoMoreLines
        Just newStack -> do
          assign' (typed @CharSourceStack) newStack

inputIsFinishedImpl :: (MonadState s m, HasType CharSourceStack s) => m Bool
inputIsFinishedImpl = use (typed @CharSourceStack) <&> CharSourceStack.stackIsFinished

-- Insert in reverse order, so, we insert the "r" of "relax" last, so we pop "r" next.
insertLexTokensImpl :: (MonadState s m, HasType CharSourceStack s) => Seq LT.LexToken -> m ()
insertLexTokensImpl lts = forM_ (Seq.reverse lts) insertLexTokenImpl

insertLexTokenImpl :: (MonadState s m, HasType CharSourceStack s) => LT.LexToken -> m ()
insertLexTokenImpl lt =
  modifying'
    (typed @CharSourceStack % CharSourceStack.currentSourceLens)
    (CharSource.insertLexTokenToSource lt)

extractNextLexTokenFromWorkingLineBuffer :: (MonadState s m, HasType CharSourceStack s) => m (Maybe LT.LexToken)
extractNextLexTokenFromWorkingLineBuffer = do
  use (typed @CharSourceStack % CharSourceStack.currentSourceLens % #workingLine % #workingLexTokens) >>= \case
    lt :<| ltRest -> do
      assign' (typed @CharSourceStack % CharSourceStack.currentSourceLens % #workingLine % #workingLexTokens) ltRest
      pure $ Just lt
    Empty ->
      pure Nothing

extractNextLexTokenFromWorkingLineSource ::
  ( MonadHexState m,
    MonadState st m,
    HasType CharSourceStack st,
    MonadError e m,
    AsType HIn.LexError e
  ) =>
  m (Maybe LT.LexToken)
extractNextLexTokenFromWorkingLineSource = do
  lineState <- use (typed @CharSourceStack % CharSourceStack.currentSourceLens % #workingLine % #sourceLine % #lineState)
  HIn.extractLexTokenFromSourceLine lineState >>= \case
    Nothing -> pure Nothing
    Just (lt, newLineState) -> do
      assign' (typed @CharSourceStack % CharSourceStack.currentSourceLens % #workingLine % #sourceLine % #lineState) newLineState
      pure $ Just lt

getNextLexTokenImpl ::
  ( MonadHexState m,
    MonadState st m,
    HasType CharSourceStack st,
    MonadError e m,
    AsType HIn.LexError e
  ) =>
  m (Maybe LT.LexToken)
getNextLexTokenImpl = do
  extractNextLexTokenFromWorkingLineBuffer >>= \case
    Just lt ->
      pure $ Just lt
    Nothing ->
      extractNextLexTokenFromWorkingLineSource >>= \case
        Just lt -> pure $ Just lt
        Nothing -> inputIsFinishedImpl >>= \case
          True -> pure Nothing
          False -> do
            endCurrentLineImpl
            getNextLexTokenImpl

openInputFileImpl ::
  ( MonadHexState m,
    MonadState st m,
    HasType CharSourceStack st,
    Env.MonadHexEnv m,
    MonadError e m,
    AsType HIn.LexError e,
    MonadIO m
  ) =>
  Q.HexFilePath ->
  m ()
openInputFileImpl inputPath = do
  absPath <-
    Env.findFilePath
      (Env.WithImplicitExtension "tex")
      (inputPath ^. typed @FilePath)
      >>= note (injectTyped (HIn.InputFileNotFound inputPath))
  newBytes <- liftIO $ BS.readFile absPath
  endLineCode <- getEndLineCharCode
  modifying' (typed @CharSourceStack) $ CharSourceStack.pushCharSource endLineCode newBytes
