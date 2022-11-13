module Hex.Run.App where

import Data.Time qualified as Time
import Effectful.FileSystem qualified as FS
import Formatting qualified as F
import Hex.Capability.Log.Impl (runLog)
import Hex.Capability.Log.Interface (HexLog (..))
import Hex.Capability.Log.Interface qualified as Log
import Hex.Common.Codes qualified as Code
import Hex.Common.HexEnv.Impl (HexEnv)
import Hex.Common.HexEnv.Impl qualified as HEnv
import Hex.Common.HexEnv.Interface (EHexEnv)
import Hex.Common.HexIO.Impl (runHexIO)
import Hex.Common.HexIO.Impl.IOState (IOState, newIOState)
import Hex.Common.HexIO.Interface (HexIO, LexError (..), fmtLexError)
import Hex.Common.HexState.Impl (runHexState)
import Hex.Common.HexState.Impl.Error qualified as HSt.Err
import Hex.Common.HexState.Impl.Scoped.GroupScopes qualified as HSt.GroupScopes
import Hex.Common.HexState.Impl.Scoped.Scope qualified as HSt.Scope
import Hex.Common.HexState.Impl.Type (HexState)
import Hex.Common.HexState.Impl.Type qualified as HSt
import Hex.Common.HexState.Interface (EHexState, ResolutionError, fmtResolutionError)
import Hex.Common.HexState.Interface.Parameter qualified as HSt.Param
import Hex.Common.TFM.Types (TFMError)
import Hex.Common.TFM.Types qualified as TFM
import Hex.Stage.Build.ListExtractor.Impl (runListExtractor)
import Hex.Stage.Build.ListExtractor.Interface (ExtractList)
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

data AppState = AppState
  { appHexState :: HSt.HexState,
    appIOState :: IOState,
    appConditionStates :: Expand.ConditionStates
  }
  deriving stock (Generic)

runInputApp ::
  (FS.FileSystem :> es, IOE :> es) =>
  HexEnv ->
  AppState ->
  Eff
    ( HexIO
        : EHexState
        : EHexEnv
        : HexLog
        : State IOState
        : State Expand.ConditionStates
        : State HexState
        : Reader HexEnv
        : Error LexError
        : Error HSt.Err.HexStateError
        : Error TFMError
        : es
    )
    (Either AppError a) ->
  Eff es (Either AppError a)
runInputApp hexEnv appState app = do
  app
    & runHexIO
    & runHexState
    & HEnv.runHexEnv
    & runLog
    & evalStateLocal appState.appIOState
    & evalStateLocal appState.appConditionStates
    & evalStateLocal appState.appHexState
    & runReader hexEnv
    & runAppErrorJoin AppLexError
    & runAppErrorJoin AppHexStateError
    & runAppErrorJoin AppTFMError

runPTSourceApp ::
  (FS.FileSystem :> es, IOE :> es) =>
  HexEnv ->
  AppState ->
  Eff
    ( Expand.PrimTokenSource
        : NonDet
        : Error ParsingError
        : Error Expand.ExpansionError
        : Error ResolutionError
        : Error EvaluationError
        : HexIO
        : EHexState
        : EHexEnv
        : HexLog
        : State IOState
        : State Expand.ConditionStates
        : State HexState
        : Reader HexEnv
        : Error LexError
        : Error HSt.Err.HexStateError
        : Error TFMError
        : es
    )
    (Either AppError a) ->
  Eff es (Either AppError a)
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
  (FS.FileSystem :> es, IOE :> es) =>
  HexEnv ->
  AppState ->
  Eff
    ( CommandSource
        : Expand.PrimTokenSource
        : NonDet
        : Error ParsingError
        : Error Expand.ExpansionError
        : Error ResolutionError
        : Error EvaluationError
        : HexIO
        : EHexState
        : EHexEnv
        : HexLog
        : State IOState
        : State Expand.ConditionStates
        : State HexState
        : Reader HexEnv
        : Error LexError
        : Error HSt.Err.HexStateError
        : Error TFMError
        : es
    )
    (Either AppError a) ->
  Eff es (Either AppError a)
runCommandSourceApp hexEnv appState app = do
  app
    & runCommandSource
    & runPTSourceApp hexEnv appState

runEvaluateApp ::
  (FS.FileSystem :> es, IOE :> es) =>
  HexEnv ->
  AppState ->
  Eff
    ( HexEvaluate
        : CommandSource
        : Expand.PrimTokenSource
        : NonDet
        : Error ParsingError
        : Error Expand.ExpansionError
        : Error ResolutionError
        : Error EvaluationError
        : HexIO
        : EHexState
        : EHexEnv
        : HexLog
        : State IOState
        : State Expand.ConditionStates
        : State HexState
        : Reader HexEnv
        : Error LexError
        : Error HSt.Err.HexStateError
        : Error TFMError
        : es
    )
    (Either AppError a) ->
  Eff es (Either AppError a)
runEvaluateApp hexEnv appState app = do
  app
    & runHexEvaluate
    & runCommandSourceApp hexEnv appState

runExtractorApp ::
  (FS.FileSystem :> es, IOE :> es) =>
  HexEnv ->
  AppState ->
  Eff
    ( ExtractList
        : Error Interpret.InterpretError
        : HexEvaluate
        : CommandSource
        : Expand.PrimTokenSource
        : NonDet
        : Error ParsingError
        : Error Expand.ExpansionError
        : Error ResolutionError
        : Error EvaluationError
        : HexIO
        : EHexState
        : EHexEnv
        : HexLog
        : State IOState
        : State Expand.ConditionStates
        : State HexState
        : Reader HexEnv
        : Error LexError
        : Error HSt.Err.HexStateError
        : Error TFMError
        : es
    )
    (Either AppError a) ->
  Eff es (Either AppError a)
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

newAppStateWithChars :: Time.ZonedTime -> Text -> ByteString -> AppState
newAppStateWithChars zonedTime name inputBytes =
  let appHexState = HSt.newHexState zonedTime
      endLineCharInt = appHexState ^. #groupScopes % #globalScope % #intParameters % at' HSt.Param.EndLineChar
      mayEndLineChar = endLineCharInt >>= Code.fromHexInt
   in AppState
        { appHexState,
          appIOState = newIOState name mayEndLineChar inputBytes,
          appConditionStates = Expand.newConditionStates
        }

data AppError
  = AppLexError LexError
  | AppHexStateError HSt.Err.HexStateError
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
  AppLexError lexError -> F.bformat fmtLexError lexError
  AppParseError parseError -> F.bformat fmtParsingError parseError
  AppExpansionError expansionError -> F.bformat Expand.fmtExpansionError expansionError
  AppInterpretError interpretError -> F.bformat Interpret.fmtInterpretError interpretError
  AppResolutionError resolutionError -> F.bformat fmtResolutionError resolutionError
  AppEvaluationError evaluationError -> F.bformat Eval.fmtEvaluationError evaluationError
  AppHexStateError hexStateError -> F.bformat HSt.Err.fmtHexStateError hexStateError
  AppTFMError tfmError -> F.bformat TFM.fmtTfmError tfmError
  AppDVIError dviError -> F.bformat Render.Spec.fmtDVIError dviError

evalAppGivenEnv ::
  Time.ZonedTime ->
  Text ->
  ByteString ->
  (HexEnv -> AppState -> m a) ->
  HexEnv ->
  m a
evalAppGivenEnv zonedTime name bs app appEnv =
  app appEnv (newAppStateWithChars zonedTime name bs)

evalApp ::
  FS.FileSystem :> es =>
  [FilePath] ->
  Log.LogLevel ->
  Time.ZonedTime ->
  Text ->
  ByteString ->
  (HexEnv -> AppState -> Eff es a) ->
  Eff es a
evalApp extraSearchDirs logLevel zonedTime name bs app =
  HEnv.withHexEnv extraSearchDirs logLevel $ \appEnv -> do
    a <- evalAppGivenEnv zonedTime name bs app appEnv
    pure a

type BasicApp a = HexEnv -> AppState -> Eff [FS.FileSystem, IOE] (Either AppError a)

evalAppIO ::
  [FilePath] ->
  Log.LogLevel ->
  Time.ZonedTime ->
  Text ->
  ByteString ->
  (HexEnv -> AppState -> Eff [FS.FileSystem, IOE] a) ->
  IO a
evalAppIO extraSearchDirs logLevel zonedTime name bs app =
  evalApp extraSearchDirs logLevel zonedTime name bs app
    & FS.runFileSystem
    & runEff

unsafeEval :: Show e => IO (Either e a) -> IO a
unsafeEval app =
  app >>= \case
    Left err -> panic $ show err
    Right v -> pure v
