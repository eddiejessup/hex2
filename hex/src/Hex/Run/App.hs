module Hex.Run.App where

import Formatting qualified as F
import Hex.Capability.Log.Interface (MonadHexLog (..))
import Hex.Capability.Log.Interface qualified as Log
import Hex.Common.Codes qualified as Code
import Hex.Common.DVI.SpecInstruction qualified as DVIS
import Hex.Common.HexEnv.Impl qualified as HEnv
import Hex.Common.HexEnv.Interface qualified as HEnv
import Hex.Common.HexInput.Impl qualified as HIn
import Hex.Common.HexInput.Impl.CharSourceStack qualified as HIn
import Hex.Common.HexInput.Interface qualified as HIn
import Hex.Common.HexState.Impl (HexStateT (..))
import Hex.Common.HexState.Impl qualified as HSt
import Hex.Common.HexState.Impl.Scoped.GroupScopes qualified as HSt.GroupScopes
import Hex.Common.HexState.Impl.Scoped.Scope qualified as HSt.Scope
import Hex.Common.HexState.Impl.Type qualified as HSt
import Hex.Common.HexState.Interface (MonadHexState)
import Hex.Common.HexState.Interface qualified as HSt
import Hex.Common.HexState.Interface.Parameter qualified as HSt.Param
import Hex.Common.Parse.Impl qualified as Parse
import Hex.Common.TFM.Types qualified as TFM
import Hex.Stage.Evaluate.Impl (EvaluateT (..))
import Hex.Stage.Evaluate.Impl.Common qualified as Eval
import Hex.Stage.Evaluate.Interface (MonadEvaluate)
import Hex.Stage.Expand.Impl (PrimTokenSourceT (..))
import Hex.Stage.Expand.Interface (MonadPrimTokenSource)
import Hex.Stage.Expand.Interface qualified as Expand
import Hex.Stage.Interpret.AllMode qualified as Interpret
import Hex.Stage.Parse.Impl (CommandSourceT (..))
import Hex.Stage.Parse.Interface (MonadCommandSource)
import Hexlude
import System.IO (hFlush)

data AppState = AppState
  { appHexState :: HSt.HexState,
    appCharSourceStack :: HIn.CharSourceStack,
    appConditionStates :: Expand.ConditionStates
  }
  deriving stock (Generic)

newAppStateWithChars :: MonadIO m => ByteString -> m AppState
newAppStateWithChars inputBytes = do
  appHexState <- HSt.newHexState
  let endLineCharInt = appHexState ^. #groupScopes % #globalScope % #intParameters % at' HSt.Param.EndLineChar
      endLineChar = endLineCharInt >>= Code.fromHexInt
  pure
    AppState
      { appHexState,
        appCharSourceStack = HIn.newCharSourceStack endLineChar inputBytes,
        appConditionStates = Expand.newConditionStates
      }

newtype App a = App {unApp :: ReaderT HEnv.HexEnv (StateT AppState (ExceptT AppError IO)) a}
  deriving newtype
    ( Functor,
      Applicative,
      Monad,
      MonadIO,
      MonadError AppError,
      MonadState AppState,
      MonadReader HEnv.HexEnv
    )
  deriving (HEnv.MonadHexEnv) via (HEnv.HexEnvT App)
  deriving (MonadHexState) via (HexStateT App)
  deriving (MonadEvaluate) via (EvaluateT App)
  deriving (HIn.MonadHexInput) via (HIn.HexInputT App)
  deriving (MonadPrimTokenSource) via (PrimTokenSourceT App)
  deriving (MonadCommandSource) via (CommandSourceT App)

data AppError
  = AppLexError HIn.LexError
  | AppParseError Parse.ParsingErrorWithContext
  | AppExpansionError Expand.ExpansionError
  | AppInterpretError Interpret.InterpretError
  | AppResolutionError HSt.ResolutionError
  | AppEvaluationError Eval.EvaluationError
  | AppHexStateError HSt.HexStateError
  | AppTFMError TFM.TFMError
  | AppDVIError DVIS.DVIError
  deriving stock (Show, Generic)

fmtAppError :: Format r (AppError -> r)
fmtAppError = F.later $ \case
  AppLexError lexError -> F.bformat HIn.fmtLexError lexError
  AppParseError parseError -> F.bformat Parse.fmtParsingErrorWithContext parseError
  AppExpansionError expansionError -> F.bformat Expand.fmtExpansionError expansionError
  AppInterpretError interpretError -> F.bformat Interpret.fmtInterpretError interpretError
  AppResolutionError resolutionError -> F.bformat HSt.fmtResolutionError resolutionError
  AppEvaluationError evaluationError -> F.bformat Eval.fmtEvaluationError evaluationError
  AppHexStateError hexStateError -> F.bformat HSt.fmtHexStateError hexStateError
  AppTFMError tfmError -> F.bformat TFM.fmtTfmError tfmError
  AppDVIError dviError -> F.bformat DVIS.fmtDVIError dviError

instance MonadHexLog App where
  log lvl msg = do
    logFileHandle <- know $ typed @Handle
    logLevel <- know $ typed @Log.LogLevel
    when (lvl >= logLevel) $ liftIO $ hPutStrLn logFileHandle $ Log.showLevelEqualWidth logLevel <> msg

  logInternalState = do
    hexState <- use $ typed @HSt.HexState
    log Log.Info $ sformat HSt.fmtHexState hexState

runAppGivenState ::
  AppState ->
  App a ->
  HEnv.HexEnv ->
  IO (Either AppError (a, AppState))
runAppGivenState appState app appEnv =
  let x = runReaderT app.unApp appEnv
   in runExceptT $ runStateT x appState

runAppGivenEnv ::
  ByteString ->
  App a ->
  HEnv.HexEnv ->
  IO (Either AppError (a, AppState))
runAppGivenEnv bs app appEnv = do
  appState <- newAppStateWithChars bs
  runAppGivenState appState app appEnv

runApp ::
  [FilePath] ->
  Log.LogLevel ->
  ByteString ->
  App a ->
  IO (Either AppError (a, AppState))
runApp extraSearchDirs logLevel bs app =
  HEnv.withHexEnv extraSearchDirs logLevel $ \appEnv -> do
    a <- runAppGivenEnv bs app appEnv
    hFlush appEnv.logHandle
    pure a

evalApp :: [FilePath] -> Log.LogLevel -> ByteString -> App a -> IO (Either AppError a)
evalApp extraSearchDirs logLevel chrs = fmap (fmap fst) <$> runApp extraSearchDirs logLevel chrs

unsafeEvalApp :: [FilePath] -> Log.LogLevel -> ByteString -> App a -> IO a
unsafeEvalApp extraSearchDirs logLevel chrs app = do
  evalApp extraSearchDirs logLevel chrs app >>= \case
    Left e -> panic $ sformat ("got error: " |%| fmtAppError) e
    Right v -> pure v
