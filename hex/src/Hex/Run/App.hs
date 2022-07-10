module Hex.Run.App where

import Data.ByteString qualified as BS
import Data.List.NonEmpty qualified as L.NE
import Formatting qualified as F
import Hex.Capability.Log.Interface (MonadHexLog (..))
import Hex.Common.Codes qualified as Code
import Hex.Common.HexEnv.Interface qualified as HEnv
import Hex.Common.HexState.Impl (MonadHexStateImplT (..))
import Hex.Common.HexState.Impl qualified as HSt
import Hex.Common.HexState.Impl.Type qualified as HSt
import Hex.Common.HexState.Interface (MonadHexState)
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
import Hex.Stage.Resolve.Impl (MonadResolveT (..))
import Hex.Stage.Resolve.Interface (MonadResolve)
import Hex.Stage.Resolve.Interface qualified as Resolve
import Hexlude
import System.IO (hFlush)
import qualified Hex.Common.HexInput.Interface as HIn
import qualified Hex.Common.HexInput.Impl as HIn
import qualified Hex.Common.HexInput.Interface.CharSource as HIn

data AppState = AppState
  { appHexState :: HSt.HexState,
    appCharSource :: HIn.LoadedCharSource,
    appConditionStates :: Expand.ConditionStates
  }
  deriving stock (Generic)

newHexStateWithChars :: NonEmpty ByteString -> AppState
newHexStateWithChars lines_ =
  let endLineChar = HSt.Param.newIntParameters ^. at' HSt.Param.EndLineChar >>= Code.fromHexInt
   in AppState
        { appHexState = HSt.newHexState,
          appCharSource = HIn.newCharSource endLineChar lines_,
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
  deriving (MonadResolve) via (MonadResolveT App)
  deriving (HIn.MonadHexInput) via (HIn.MonadHexInputT App)
  deriving (MonadPrimTokenSource) via (MonadPrimTokenSourceT App)
  deriving (MonadCommandSource) via (MonadCommandSourceT App)

data AppError
  = AppLexError HIn.LexError
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
  AppLexError lexError -> F.bformat HIn.fmtLexError lexError
  AppParseError parseError -> F.bformat Parse.fmtParsingError parseError
  AppExpansionError expansionError -> F.bformat Expand.fmtExpansionError expansionError
  AppInterpretError interpretError -> F.bformat Interpret.fmtInterpretError interpretError
  AppResolutionError resolutionError -> F.bformat Resolve.fmtResolutionError resolutionError
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
  NonEmpty ByteString ->
  App a ->
  HEnv.HexEnv ->
  IO (Either AppError (a, AppState))
runAppGivenEnv bs app appEnv = do
  let appState = newHexStateWithChars bs
  runAppGivenState appState app appEnv

runApp ::
  NonEmpty ByteString ->
  App a ->
  IO (Either AppError (a, AppState))
runApp bs app =
  HEnv.withHexEnv $ \appEnv -> do
    a <- runAppGivenEnv bs app appEnv
    hFlush appEnv.logHandle
    pure a

evalApp :: NonEmpty ByteString -> App a -> IO (Either AppError a)
evalApp chrs = fmap (fmap fst) <$> runApp chrs

unsafeEvalApp :: NonEmpty ByteString -> App a -> IO a
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

-- Split the input into lines, assuming '\n' line-termination characters.
-- We will append the \endlinechar to each input line as we traverse the lines.
-- We need at least one input line, to be the 'current line'.
toInputLines :: ByteString -> Maybe (NonEmpty ByteString)
toInputLines inputBytes = L.NE.nonEmpty (BS.split newlineWord inputBytes)
