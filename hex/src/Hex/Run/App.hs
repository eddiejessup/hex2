module Hex.Run.App where

import Formatting qualified as F
import Hex.Capability.Log.Interface (MonadHexLog (..))
import Hex.Common.HexState.Impl (MonadHexStateImplT (..))
import Hex.Common.HexState.Impl qualified as HSt
import Hex.Common.HexState.Impl.Type qualified as HSt
import Hex.Common.HexState.Interface (MonadHexState)
import Hex.Common.Parse.Interface qualified as Parse
import Hex.Common.TFM.Get qualified as TFM
import Hex.Stage.Categorise.Impl (MonadCharCatSourceT (..))
import Hex.Stage.Categorise.Interface (MonadCharCatSource)
import Hex.Stage.Evaluate.Impl (MonadEvaluateT (..))
import Hex.Stage.Evaluate.Impl.Common qualified as Eval
import Hex.Stage.Evaluate.Interface (MonadEvaluate)
import Hex.Stage.Expand.Impl (MonadPrimTokenSourceT (..))
import Hex.Stage.Expand.Interface (MonadPrimTokenSource)
import Hex.Stage.Expand.Interface qualified as Expand
import Hex.Stage.Interpret.CommandHandler.AllMode qualified as Interpret
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
import qualified Hex.Stage.Resolve.Interface as Resolve

data AppEnv = AppEnv
  { appLogHandle :: Handle,
    searchDirectories :: [FilePath]
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

data AppError
  = AppLexError Lex.LexError
  | AppParseError Parse.ParsingError
  | AppExpansionError Expand.ExpansionError
  | AppInterpretError Interpret.InterpretError
  | AppResolutionError Resolve.ResolutionError
  | AppEvaluationError Eval.EvaluationError
  | AppHexStateError HSt.HexStateError
  | AppTFMError TFM.TFMError
  deriving stock (Show, Generic)

instance {-# OVERLAPPING #-} AsType Parse.ParseUnexpectedError AppError where
  _Typed = _Typed @Parse.ParsingError % _Typed @Parse.ParseUnexpectedError

fmtAppError :: Format r (AppError -> r)
fmtAppError = F.later $ \case
  AppLexError lexError -> F.bformat Lex.fmtLexError lexError
  AppParseError parseError -> F.bformat Parse.fmtParsingError parseError
  AppExpansionError expansionError -> F.bformat Expand.fmtExpansionError expansionError
  AppInterpretError interpretError -> F.bformat Interpret.fmtInterpretError interpretError
  AppResolutionError resolutionError -> F.bformat Resolve.fmtResolutionError resolutionError
  AppEvaluationError evaluationError -> F.bformat Eval.fmtEvaluationError evaluationError
  AppHexStateError hexStateError -> F.bformat HSt.fmtHexStateError hexStateError
  AppTFMError tfmError -> F.bformat TFM.fmtTfmError tfmError

instance MonadHexLog App where
  logText msg = do
    logFileHandle <- know $ typed @Handle
    liftIO $ hPutStrLn logFileHandle msg

  logInternalState = do
    logFileHandle <- know $ typed @Handle
    hexState <- use $ typed @HSt.HexState
    let msg = sformat HSt.fmtHexState hexState
    liftIO $ hPutStrLn logFileHandle msg

runAppGivenState ::
  AppState ->
  App a ->
  AppEnv ->
  IO (Either AppError (a, AppState))
runAppGivenState appState app appEnv =
  let x = runReaderT app.unApp appEnv
   in runExceptT $ runStateT x appState

newAppEnv :: Handle -> AppEnv
newAppEnv appLogHandle =
  AppEnv {appLogHandle, searchDirectories = []}

-- | Set up the enironment in this 'with'-style, to ensure we clean up the log
-- handle when we're finished.
withAppEnv :: (AppEnv -> IO a) -> IO a
withAppEnv k = do
  withFile "log.txt" WriteMode $ \appLogHandle -> do
    k $ newAppEnv appLogHandle

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
    Left e -> panic $ sformat ("got error: " |%| fmtAppError) e
    Right v -> pure v
