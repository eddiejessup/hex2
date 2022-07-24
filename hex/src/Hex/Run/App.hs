module Hex.Run.App where

import Formatting qualified as F
import Hex.Capability.Log.Impl (runLog)
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
import Hex.Common.HexState.Interface (EHexState, ResolutionError, fmtResolutionError)
import Hex.Common.HexState.Interface.Parameter qualified as HSt.Param
import Hex.Common.TFM.Types (TFMError)
import Hex.Common.TFM.Types qualified as TFM
import Hex.Stage.Build.ListExtractor.HList (runListExtractor)
import Hex.Stage.Build.ListExtractor.Interface (ExtractHList)
import Hex.Stage.Evaluate.Impl (runHexEvaluate)
import Hex.Stage.Evaluate.Impl.Common (EvaluationError)
import Hex.Stage.Evaluate.Impl.Common qualified as Eval
import Hex.Stage.Evaluate.Interface (HexEvaluate)
import Hex.Stage.Expand.Impl (runAlt, runPrimTokenSource)
import Hex.Stage.Expand.Interface (ParsingError, fmtParsingError)
import Hex.Stage.Expand.Interface qualified as Expand
import Hex.Stage.Interpret.AllMode qualified as Interpret
import Hex.Stage.Parse.Impl (runCommandSource)
import Hex.Stage.Parse.Interface (CommandSource)
import Hex.Stage.Render.Interface.SpecInstruction qualified as Render.Spec
import Hexlude
import System.IO (hFlush)

data AppState = AppState
  { appHexState :: HSt.HexState,
    appCharSourceStack :: HIn.CharSourceStack,
    appConditionStates :: Expand.ConditionStates
  }
  deriving stock (Generic)

runInputApp ::
  HexEnv ->
  AppState ->
  Eff ([HexInput, EHexState, EHexEnv, HexLog, State HIn.CharSourceStack, State Expand.ConditionStates, State HexState, Reader HexEnv, Error LexError, Error HexStateError, Error TFMError, IOE]) (Either AppError a) ->
  IO (Either AppError a)
runInputApp hexEnv appState app = do
  app
    & runHexInput
    & runHexState
    & runHexEnv
    & runLog
    & evalStateLocal appState.appCharSourceStack
    & evalStateLocal appState.appConditionStates
    & evalStateLocal appState.appHexState
    & runReader hexEnv
    & runAppErrorJoin AppLexError
    & runAppErrorJoin AppHexStateError
    & runAppErrorJoin AppTFMError
    & runEff

runPTSourceApp ::
  HexEnv ->
  AppState ->
  Eff '[Expand.PrimTokenSource, EAlternative, Error ParsingError, Error Expand.ExpansionError, Error ResolutionError, Error EvaluationError, HexInput, EHexState, EHexEnv, HexLog, State HIn.CharSourceStack, State Expand.ConditionStates, State HexState, Reader HexEnv, Error LexError, Error HexStateError, Error TFMError, IOE] (Either AppError a) ->
  IO (Either AppError a)
runPTSourceApp hexEnv appState app = do
  app
    & runPrimTokenSource
    & runAlt
    & runAppErrorJoin AppParseError
    & runAppErrorJoin AppExpansionError
    & runAppErrorJoin AppResolutionError
    & runAppErrorJoin AppEvaluationError
    & runInputApp hexEnv appState

runCommandSourceApp ::
  HexEnv ->
  AppState ->
  Eff '[CommandSource, Expand.PrimTokenSource, EAlternative, Error ParsingError, Error Expand.ExpansionError, Error ResolutionError, Error EvaluationError, HexInput, EHexState, EHexEnv, HexLog, State HIn.CharSourceStack, State Expand.ConditionStates, State HexState, Reader HexEnv, Error LexError, Error HexStateError, Error TFMError, IOE] (Either AppError a) ->
  IO (Either AppError a)
runCommandSourceApp hexEnv appState app = do
  app
    & runCommandSource
    & runPTSourceApp hexEnv appState

runEvaluateApp ::
  HexEnv ->
  AppState ->
  Eff '[HexEvaluate, CommandSource, Expand.PrimTokenSource, EAlternative, Error ParsingError, Error Expand.ExpansionError, Error ResolutionError, Error EvaluationError, HexInput, EHexState, EHexEnv, HexLog, State HIn.CharSourceStack, State Expand.ConditionStates, State HexState, Reader HexEnv, Error LexError, Error HexStateError, Error TFMError, IOE] (Either AppError a) ->
  IO (Either AppError a)
runEvaluateApp hexEnv appState app = do
  app
    & runHexEvaluate
    & runCommandSourceApp hexEnv appState

runExtractorApp ::
  HexEnv ->
  AppState ->
  Eff '[ExtractHList, Error Interpret.InterpretError, HexEvaluate, CommandSource, Expand.PrimTokenSource, EAlternative, Error ParsingError, Error Expand.ExpansionError, Error ResolutionError, Error EvaluationError, HexInput, EHexState, EHexEnv, HexLog, State HIn.CharSourceStack, State Expand.ConditionStates, State HexState, Reader HexEnv, Error LexError, Error HexStateError, Error TFMError, IOE] (Either AppError a) ->
  IO (Either AppError a)
runExtractorApp hexEnv appState app = do
  app
    & runListExtractor
    & runAppErrorJoin AppInterpretError
    & runEvaluateApp hexEnv appState

runAppError :: forall e es a. (e -> AppError) -> Eff (Error e : es) a -> Eff es (Either AppError a)
runAppError inj eff = do
  runErrorNoCallStack @e eff >>= \case
    Left e -> pure $ Left $ inj e
    Right v -> pure $ Right v

runAppErrorJoin :: forall e es a. (e -> AppError) -> Eff (Error e : es) (Either AppError a) -> Eff es (Either AppError a)
runAppErrorJoin inj eff = do
  runErrorNoCallStack @e eff >>= \case
    Left e -> pure $ Left $ inj e
    Right v -> pure v

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
  | AppParseError ParsingError
  | AppExpansionError Expand.ExpansionError
  | AppInterpretError Interpret.InterpretError
  | AppResolutionError ResolutionError
  | AppEvaluationError Eval.EvaluationError
  | AppDVIError Render.Spec.DVIError
  deriving stock (Show, Generic)

fmtAppError :: Format r (AppError -> r)
fmtAppError = F.later $ \case
  AppLexError lexError -> F.bformat HIn.fmtLexError lexError
  AppParseError parseError -> F.bformat fmtParsingError parseError
  AppExpansionError expansionError -> F.bformat Expand.fmtExpansionError expansionError
  AppInterpretError interpretError -> F.bformat Interpret.fmtInterpretError interpretError
  AppResolutionError resolutionError -> F.bformat fmtResolutionError resolutionError
  AppEvaluationError evaluationError -> F.bformat Eval.fmtEvaluationError evaluationError
  AppHexStateError hexStateError -> F.bformat HSt.fmtHexStateError hexStateError
  AppTFMError tfmError -> F.bformat TFM.fmtTfmError tfmError
  AppDVIError dviError -> F.bformat Render.Spec.fmtDVIError dviError

evalAppGivenEnv ::
  ByteString ->
  (HEnv.HexEnv -> AppState -> IO a) ->
  HEnv.HexEnv ->
  IO a
evalAppGivenEnv bs app appEnv =
  newAppStateWithChars bs >>= app appEnv

evalApp ::
  [FilePath] ->
  Log.LogLevel ->
  ByteString ->
  (HEnv.HexEnv -> AppState -> IO a) ->
  IO a
evalApp extraSearchDirs logLevel bs app =
  HEnv.withHexEnv extraSearchDirs logLevel $ \appEnv -> do
    a <- evalAppGivenEnv bs app appEnv
    hFlush appEnv.logHandle
    pure a

unsafeEvalApp :: [FilePath] -> Log.LogLevel -> ByteString -> (HEnv.HexEnv -> AppState -> IO (Either AppError a)) -> IO a
unsafeEvalApp extraSearchDirs logLevel chrs app =
  evalApp extraSearchDirs logLevel chrs app >>= \case
    Left err -> panic $ "unsafeEvalApp: " <> show err
    Right v -> pure v
