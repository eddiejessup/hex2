module Hex.Run.App where

import Hex.Capability.Log.Interface (HexLog (..))
import Hex.Capability.Log.Interface qualified as Log
import Hex.Common.Codes qualified as Code
import Hex.Common.HexEnv.Impl (HexEnv, runHexEnv)
import Hex.Common.HexEnv.Impl qualified as HEnv
import Hex.Common.HexEnv.Interface (EHexEnv)
import Hex.Common.HexInput.Impl (runHexInput)
import Hex.Common.HexInput.Impl.CharSourceStack qualified as HIn
import Hex.Common.HexInput.Interface (HexInput, LexError)
import Hex.Common.HexInput.Interface qualified as HIn
import Hex.Common.HexState.Impl (HexStateError, runHexState)
import Hex.Common.HexState.Impl qualified as HSt
import Hex.Common.HexState.Impl.Scoped.GroupScopes qualified as HSt.GroupScopes
import Hex.Common.HexState.Impl.Scoped.Scope qualified as HSt.Scope
import Hex.Common.HexState.Impl.Type (HexState)
import Hex.Common.HexState.Impl.Type qualified as HSt
import Hex.Common.HexState.Interface (EHexState, ResolutionError)
import Hex.Common.HexState.Interface.Parameter qualified as HSt.Param
import Hex.Common.TFM.Types (TFMError)
import Hex.Common.TFM.Types qualified as TFM
import Hex.Stage.Expand.Interface qualified as Expand
import Hexlude
import System.IO (hFlush)
import Hex.Stage.Expand.Impl (runPrimTokenSource)
import Hex.Common.Parse.Impl (runAlt, ParsingError)
import Hex.Stage.Evaluate.Impl.Common (EvaluationError)

data AppState = AppState
  { appHexState :: HSt.HexState,
    appCharSourceStack :: HIn.CharSourceStack,
    appConditionStates :: Expand.ConditionStates
  }
  deriving stock (Generic)

type App a = Eff [HexInput, EHexState, EHexEnv, HexLog, State HIn.CharSourceStack, State Expand.ConditionStates, State HexState, Reader HexEnv, Error LexError, Error HexStateError, Error TFMError, IOE] a

runInputAppRaw ::
  HexEnv ->
  AppState ->
  Eff ([HexInput, EHexState, EHexEnv, HexLog, State HIn.CharSourceStack, State Expand.ConditionStates, State HexState, Reader HexEnv, Error LexError, Error HexStateError, Error TFMError, IOE]) a ->
  IO (Either TFMError (Either HexStateError (Either LexError a)))
runInputAppRaw hexEnv appState app = do
  app
    & runHexInput
    & runHexState
    & runHexEnv
    & runLog
    & evalStateLocal appState.appCharSourceStack
    & evalStateLocal appState.appConditionStates
    & evalStateLocal appState.appHexState
    & runReader hexEnv
    & runErrorNoCallStack @LexError
    & runErrorNoCallStack @HSt.HexStateError
    & runErrorNoCallStack @TFM.TFMError
    & runEff

runInputApp ::
  HexEnv ->
  AppState ->
  Eff [HexInput, EHexState, EHexEnv, HexLog, State HIn.CharSourceStack, State Expand.ConditionStates, State HexState, Reader HexEnv, Error LexError, Error HexStateError, Error TFMError, IOE] a ->
  IO (Either AppError a)
runInputApp hexEnv appState app = do
  runInputAppRaw hexEnv appState app <&> \case
    Left err -> Left $ AppTFMError err
    Right (Left err) -> Left $ AppHexStateError err
    Right (Right (Left err)) -> Left $ AppLexError err
    Right (Right (Right v)) -> Right v

runPTSource ::
  Eff '[Expand.PrimTokenSource, EHexState, EAlternative, Error ParsingError, Error Expand.ExpansionError, Error ResolutionError, Error EvaluationError, IOE] a ->
  IO (Either ParsingError a)
runPTSource app = do
  app
    & runPrimTokenSource
    & runHexState
    & runAlt
    & runErrorNoCallStack @ParsingError
    & runErrorNoCallStack @Expand.ExpansionError
    & runErrorNoCallStack @ResolutionError
    & runErrorNoCallStack @EvaluationError
    & runEff

newAppStateWithChars :: ByteString -> IO AppState
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

data AppError
  = AppLexError HIn.LexError
  | AppHexStateError HSt.HexStateError
  | AppTFMError TFM.TFMError
  --   | AppParseError Parse.ParsingErrorWithContext
  --   | AppExpansionError Expand.ExpansionError
  --   | AppInterpretError Interpret.InterpretError
  --   | AppResolutionError HSt.ResolutionError
  --   | AppEvaluationError Eval.EvaluationError
  --   | AppDVIError DVIS.DVIError
  deriving stock (Show, Generic)

-- fmtAppError :: Format r (AppError -> r)
-- fmtAppError = F.later $ \case
--   AppLexError lexError -> F.bformat HIn.fmtLexError lexError
--   AppParseError parseError -> F.bformat Parse.fmtParsingErrorWithContext parseError
--   AppExpansionError expansionError -> F.bformat Expand.fmtExpansionError expansionError
--   AppInterpretError interpretError -> F.bformat Interpret.fmtInterpretError interpretError
--   AppResolutionError resolutionError -> F.bformat HSt.fmtResolutionError resolutionError
--   AppEvaluationError evaluationError -> F.bformat Eval.fmtEvaluationError evaluationError
--   AppHexStateError hexStateError -> F.bformat HSt.fmtHexStateError hexStateError
--   AppTFMError tfmError -> F.bformat TFM.fmtTfmError tfmError
--   AppDVIError dviError -> F.bformat DVIS.fmtDVIError dviError

runLog :: [IOE, Reader HexEnv, State HexState] :>> es => Eff (HexLog : es) a -> Eff es a
runLog = interpret $ \_ -> \case
  Log.Log lvl msg -> logImpl lvl msg
  Log.LogInternalState -> do
    hexState <- get @HexState
    logImpl Log.Info $ sformat HSt.fmtHexState hexState

logImpl :: [IOE, Reader HexEnv, State HexState] :>> es => Log.LogLevel -> Text -> Eff es ()
logImpl lvl msg = do
  logFileHandle <- know @HexEnv $ typed @Handle
  logLevel <- know @HexEnv $ typed @Log.LogLevel
  when (lvl >= logLevel) $ liftIO $ hPutStrLn logFileHandle $ Log.showLevelEqualWidth logLevel <> msg

evalAppGivenState ::
  AppState ->
  App a ->
  HEnv.HexEnv ->
  IO (Either AppError a)
evalAppGivenState appState app appEnv = do
  runInputApp appEnv appState app

evalAppGivenEnv ::
  ByteString ->
  App a ->
  HEnv.HexEnv ->
  IO (Either AppError a)
evalAppGivenEnv bs app appEnv = do
  appState <- newAppStateWithChars bs
  evalAppGivenState appState app appEnv

evalApp ::
  [FilePath] ->
  Log.LogLevel ->
  ByteString ->
  App a ->
  IO (Either AppError a)
evalApp extraSearchDirs logLevel bs app =
  HEnv.withHexEnv extraSearchDirs logLevel $ \appEnv -> do
    a <- evalAppGivenEnv bs app appEnv
    hFlush appEnv.logHandle
    pure a

unsafeEvalApp :: [FilePath] -> Log.LogLevel -> ByteString -> App a -> IO a
unsafeEvalApp extraSearchDirs logLevel chrs app =
  evalApp extraSearchDirs logLevel chrs app >>= \case
    Left err -> panic $ "unsafeEvalApp: " <> show err
    Right v -> pure v
