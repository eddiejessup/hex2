module Hex.Run.App where

import Hex.Capability.Log.Impl (MonadHexLogT (..))
import Hex.Capability.Log.Interface (MonadHexLog)
import Hex.Common.HexState.Impl (HexStateError, MonadHexStateImplT (..))
import Hex.Common.HexState.Impl.Type qualified as HSt
import Hex.Common.HexState.Interface (MonadHexState)
import Hex.Common.Parse (ParseUnexpectedError)
import Hex.Common.TFM.Get qualified as H.TFM
import Hex.Stage.Categorise.Impl (MonadCharCatSourceT (..))
import Hex.Stage.Categorise.Interface (MonadCharCatSource)
import Hex.Stage.Evaluate.Impl (MonadEvaluateT (..))
import Hex.Stage.Evaluate.Impl.Common (EvaluationError)
import Hex.Stage.Evaluate.Interface (MonadEvaluate)
import Hex.Stage.Expand.Impl (ExpansionError, MonadPrimTokenSourceT (..))
import Hex.Stage.Expand.Interface (MonadPrimTokenSource)
import Hex.Stage.Interpret.CommandHandler.AllMode (InterpretError)
import Hex.Stage.Lex.Impl (MonadLexTokenSourceT (..))
import Hex.Stage.Lex.Interface (MonadLexTokenSource)
import Hex.Stage.Lex.Interface.CharSource (CharSource, newCharSource)
import Hex.Stage.Lex.Interface.Extract qualified as Lex
import Hex.Stage.Parse.Impl (MonadCommandSourceT (..))
import Hex.Stage.Parse.Interface (MonadCommandSource)
import Hex.Stage.Resolve.Impl (MonadResolveT (..))
import Hex.Stage.Resolve.Interface (MonadResolve)
import Hexlude
import System.IO (hFlush)

data AppEnv = AppEnv
  { appLogHandle :: Handle
  }
  deriving stock (Generic)

data AppState = AppState
  { appHexState :: HSt.HexState,
    appCharSource :: CharSource
  }
  deriving stock (Generic)

instance {-# OVERLAPPING #-} HasType ByteString AppState where
  typed = typed @CharSource % typed @ByteString

newHexStateWithChars :: ByteString -> AppState
newHexStateWithChars chrs = AppState HSt.newHexState (newCharSource chrs)

newtype App a = App {unApp :: ReaderT AppEnv (StateT AppState (ExceptT AppError IO)) a}
  deriving newtype
    ( Functor,
      Applicative,
      Monad,
      MonadIO,
      MonadError AppError,
      MonadState AppState,
      MonadReader AppEnv
    )
  deriving (MonadHexState) via (MonadHexStateImplT App)
  deriving (MonadCharCatSource) via (MonadCharCatSourceT App)
  deriving (MonadEvaluate) via (MonadEvaluateT App)
  deriving (MonadResolve) via (MonadResolveT App)
  deriving (MonadLexTokenSource) via (MonadLexTokenSourceT App)
  deriving (MonadPrimTokenSource) via (MonadPrimTokenSourceT App)
  deriving (MonadCommandSource) via (MonadCommandSourceT App)
  deriving (MonadHexLog) via (MonadHexLogT App)

data AppError
  = AppLexError Lex.LexError
  | AppParseError ParseUnexpectedError
  | AppExpansionError ExpansionError
  | AppInterpretError InterpretError
  | AppEvaluationError EvaluationError
  | AppHexStateError HexStateError
  | AppTFMError H.TFM.TFMError
  deriving stock (Generic, Show)

runAppGivenState ::
  AppState ->
  App a ->
  AppEnv ->
  IO (Either AppError (a, AppState))
runAppGivenState appState app appEnv =
  let x = runReaderT app.unApp appEnv
   in runExceptT $ runStateT x appState

-- | Set up the enironment in this 'with'-style, to ensure we clean up the log
-- handle when we're finished.
withAppEnv :: (AppEnv -> IO a) -> IO a
withAppEnv k = do
  appLogHandle <- openFile "log.txt" WriteMode
  k $ AppEnv {appLogHandle}

runAppGivenEnv ::
  ByteString ->
  App a ->
  AppEnv ->
  IO (Either AppError (a, AppState))
runAppGivenEnv bs app appEnv = do
  let appState = newHexStateWithChars bs
  runAppGivenState appState app appEnv

runApp ::
  ByteString ->
  App a ->
  IO (Either AppError (a, AppState))
runApp bs app =
  withAppEnv $ \appEnv -> do
    a <- runAppGivenEnv bs app appEnv
    hFlush appEnv.appLogHandle
    pure a

evalApp :: ByteString -> App a -> IO (Either AppError a)
evalApp chrs = fmap (fmap fst) <$> runApp chrs

unsafeEvalApp :: ByteString -> App a -> IO a
unsafeEvalApp chrs app = do
  evalApp chrs app >>= \case
    Left e -> panic $ "got error: " <> show e
    Right v -> pure v
