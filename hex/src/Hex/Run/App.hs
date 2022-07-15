module Hex.Run.App where

import Formatting qualified as F
import Hex.Capability.Log.Interface (MonadHexLog (..))
import Hex.Common.Codes qualified as Code
import Hex.Common.HexEnv.Interface qualified as HEnv
import Hex.Common.HexInput.Impl qualified as HIn
import Hex.Common.HexInput.Impl.CharSourceStack qualified as HIn
import Hex.Common.HexInput.Interface qualified as HIn
import Hex.Common.HexState.Impl (MonadHexStateImplT (..))
import Hex.Common.HexState.Impl qualified as HSt
import Hex.Common.HexState.Impl.Scoped.GroupScopes qualified as HSt.GroupScopes
import Hex.Common.HexState.Impl.Scoped.Scope qualified as HSt.Scope
import Hex.Common.HexState.Impl.Type qualified as HSt
import Hex.Common.HexState.Interface (MonadHexState)
import Hex.Common.HexState.Interface qualified as HSt
import Hex.Common.HexState.Interface.Parameter qualified as HSt.Param
import Hex.Common.Parse.Interface qualified as Parse
import Hex.Common.TFM.Get qualified as TFM
import Hex.Stage.Evaluate.Impl (MonadEvaluateT (..))
import Hex.Stage.Evaluate.Impl.Common qualified as Eval
import Hex.Stage.Evaluate.Interface (MonadEvaluate)
import Hex.Stage.Expand.Impl (MonadPrimTokenSourceT (..))
import Hex.Stage.Expand.Interface (MonadPrimTokenSource)
import Hex.Stage.Expand.Interface qualified as Expand
import Hex.Stage.Interpret.CommandHandler.AllMode qualified as Interpret
import Hex.Stage.Parse.Impl (MonadCommandSourceT (..))
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
  deriving (MonadHexState) via (MonadHexStateImplT App)
  deriving (MonadEvaluate) via (MonadEvaluateT App)
  deriving (HIn.MonadHexInput) via (HIn.MonadHexInputT App)
  deriving (MonadPrimTokenSource) via (MonadPrimTokenSourceT App)
  deriving (MonadCommandSource) via (MonadCommandSourceT App)

data AppError
  = AppLexError HIn.LexError
  | AppParseError Parse.ParsingError
  | AppExpansionError Expand.ExpansionError
  | AppInterpretError Interpret.InterpretError
  | AppResolutionError HSt.ResolutionError
  | AppEvaluationError Eval.EvaluationError
  | AppHexStateError HSt.HexStateError
  | AppTFMError TFM.TFMError
  deriving stock (Show, Generic)

instance {-# OVERLAPPING #-} AsType Parse.ParseUnexpectedError AppError where
  _Typed = _Typed @Parse.ParsingError % _Typed @Parse.ParseUnexpectedError

fmtAppError :: Format r (AppError -> r)
fmtAppError = F.later $ \case
  AppLexError lexError -> F.bformat HIn.fmtLexError lexError
  AppParseError parseError -> F.bformat Parse.fmtParsingError parseError
  AppExpansionError expansionError -> F.bformat Expand.fmtExpansionError expansionError
  AppInterpretError interpretError -> F.bformat Interpret.fmtInterpretError interpretError
  AppResolutionError resolutionError -> F.bformat HSt.fmtResolutionError resolutionError
  AppEvaluationError evaluationError -> F.bformat Eval.fmtEvaluationError evaluationError
  AppHexStateError hexStateError -> F.bformat HSt.fmtHexStateError hexStateError
  AppTFMError tfmError -> F.bformat TFM.fmtTfmError tfmError

instance MonadHexLog App where
  log msg = do
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
  ByteString ->
  App a ->
  IO (Either AppError (a, AppState))
runApp bs app =
  HEnv.withHexEnv $ \appEnv -> do
    a <- runAppGivenEnv bs app appEnv
    hFlush appEnv.logHandle
    pure a

evalApp :: ByteString -> App a -> IO (Either AppError a)
evalApp chrs = fmap (fmap fst) <$> runApp chrs

unsafeEvalApp :: ByteString -> App a -> IO a
unsafeEvalApp chrs app = do
  evalApp chrs app >>= \case
    Left e -> panic $ sformat ("got error: " |%| fmtAppError) e
    Right v -> pure v

newlineWord :: Word8
newlineWord = 10

carriageReturnWord :: Word8
carriageReturnWord = 13

carriageReturnCharCode :: Code.CharCode
carriageReturnCharCode = Code.CharCode carriageReturnWord
