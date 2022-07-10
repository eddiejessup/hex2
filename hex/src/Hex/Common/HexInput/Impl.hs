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

newtype MonadHexInputT m a = MonadHexInputT {unMonadHexInputT :: m a}
  deriving newtype
    ( Functor,
      Applicative,
      Monad,
      MonadIO,
      MonadState st,
      MonadError e,
      MonadHexState,
      MonadHexLog
    )

instance
  ( Monad m,
    MonadError e (MonadHexInputT m),
    MonadState st m,
    HasType CharSource.LoadedCharSource st,
    AsType HIn.LexError e,
    MonadHexState (MonadHexInputT m)
  ) =>
  MonadHexInput (MonadHexInputT m)
  where
  endCurrentLine = endCurrentLineImpl

  sourceIsFinished = sourceIsFinishedImpl

  getSource = use (typed @CharSource.LoadedCharSource)

  putSource = assign' (typed @CharSource.LoadedCharSource)

  insertLexToken = insertLexTokenImpl

  insertLexTokens = insertLexTokensImpl

  getNextLexToken = getNextLexTokenImpl

getEndLineCharCode :: HSt.MonadHexState m => m (Maybe Code.CharCode)
getEndLineCharCode =
  Code.fromHexInt <$> HSt.getParameterValue (HSt.Param.IntQuantParam HSt.Param.EndLineChar)

endCurrentLineImpl ::
  ( MonadState st m,
    HasType CharSource.LoadedCharSource st,
    HSt.MonadHexState m,
    MonadError e m,
    AsType HIn.LexError e
  ) =>
  m ()
endCurrentLineImpl = do
  loadedCharSource <- use (typed @CharSource.LoadedCharSource)
  -- End the line, update the char-source to the new state
  CharSource.endSourceCurrentLine <$> getEndLineCharCode <*> pure loadedCharSource >>= \case
    Nothing -> throwError $ injectTyped HIn.NoMoreLines
    Just newSource -> assign' (typed @CharSource.LoadedCharSource) newSource

sourceIsFinishedImpl ::
  ( MonadState st m,
    HasType CharSource.LoadedCharSource st
  ) =>
  m Bool
sourceIsFinishedImpl = do
  CharSource.sourceIsFinished <$> use (typed @CharSource.LoadedCharSource)

-- Insert in reverse order, so, we insert the "r" of "relax" last, so we pop "r" next.
insertLexTokensImpl :: (HasType CharSource.LoadedCharSource s, MonadState s m) => Seq LT.LexToken -> m ()
insertLexTokensImpl lts = forM_ (Seq.reverse lts) insertLexTokenImpl

insertLexTokenImpl :: (HasType CharSource.LoadedCharSource s, MonadState s m) => LT.LexToken -> m ()
insertLexTokenImpl lt =
  modifying'
    (typed @CharSource.LoadedCharSource)
    (CharSource.insertLexTokenToSource lt)

extractNextLexTokenFromWorkingLineBuffer :: (HasType CharSource.LoadedCharSource s, MonadState s m) => m (Maybe LT.LexToken)
extractNextLexTokenFromWorkingLineBuffer = do
  use (typed @CharSource.LoadedCharSource % #workingLine % #workingLexTokens) >>= \case
    lt :<| ltRest -> do
      assign' (typed @CharSource.LoadedCharSource % #workingLine % #workingLexTokens) ltRest
      pure $ Just lt
    Empty -> pure
      Nothing

extractNextLexTokenFromWorkingLineSource ::
  ( MonadHexState m,
    MonadState st m,
    HasType CharSource.LoadedCharSource st,
    MonadError e m,
    AsType HIn.LexError e
  ) =>
  m (Maybe LT.LexToken)
extractNextLexTokenFromWorkingLineSource = do
  lineState <- use (typed @CharSource.LoadedCharSource % #workingLine % #sourceLine % #lineState)
  HIn.extractLexTokenFromSourceLine lineState >>= \case
    Nothing -> pure Nothing
    Just (lt, newLineState) -> do
      assign' (typed @CharSource.LoadedCharSource % #workingLine % #sourceLine % #lineState) newLineState
      pure $ Just lt

getNextLexTokenImpl ::
  ( MonadHexState m,
    MonadState st m,
    HasType CharSource.LoadedCharSource st,
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
        Nothing -> sourceIsFinishedImpl >>= \case
          True -> pure Nothing
          False -> do
            endCurrentLineImpl
            getNextLexTokenImpl
