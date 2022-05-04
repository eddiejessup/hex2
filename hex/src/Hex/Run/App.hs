module Hex.Run.App where

import Hex.Common.HexState.Impl (HexStateError, MonadHexStateImplT (..))
import Hex.Common.HexState.Impl.Type qualified as H.St
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

data HexStateWithChars = HexStateWithChars H.St.HexState CharSource
  deriving stock (Generic)

instance {-# OVERLAPPING #-} HasType ByteString HexStateWithChars where
  typed = typed @CharSource % typed @ByteString

newHexStateWithChars :: ByteString -> HexStateWithChars
newHexStateWithChars chrs = HexStateWithChars H.St.newHexState (newCharSource chrs)

newtype App a = App {unApp :: StateT HexStateWithChars (ExceptT AppError IO) a}
  deriving newtype
    ( Functor,
      Applicative,
      Monad,
      MonadIO,
      MonadError AppError,
      MonadState HexStateWithChars
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
  | AppParseError ParseUnexpectedError
  | AppExpansionError ExpansionError
  | AppInterpretError InterpretError
  | AppEvaluationError EvaluationError
  | AppHexStateError HexStateError
  | AppTFMError H.TFM.TFMError
  deriving stock (Generic, Show)

runAppWithState ::
  HexStateWithChars ->
  App a ->
  IO (Either AppError (a, HexStateWithChars))
runAppWithState chrSrc app = runExceptT (runStateT (app.unApp) chrSrc)

runApp ::
  ByteString ->
  App a ->
  IO (Either AppError (a, HexStateWithChars))
runApp = runAppWithState . newHexStateWithChars

evalApp :: ByteString -> App a -> IO (Either AppError a)
evalApp chrs = fmap (fmap fst) <$> runApp chrs

unsafeEvalApp :: ByteString -> App a -> IO a
unsafeEvalApp chrs app = do
  evalApp chrs app >>= \case
    Left e -> panic $ "got error: " <> show e
    Right v -> pure v
