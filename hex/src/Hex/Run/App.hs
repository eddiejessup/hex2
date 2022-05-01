module Hex.Run.App where

import Hex.Common.HexState.Impl (HexStateError)
import Hex.Common.HexState.Impl.Type qualified as H.St
import Hex.Common.Parse (ParsingError, ParseUnexpectedError)
import Hex.Common.TFM.Get qualified as H.TFM
import Hex.Stage.Evaluate.Impl (EvaluationError)
import Hex.Stage.Expand.Impl (ExpansionError)
import Hex.Stage.Interpret.CommandHandler.AllMode (InterpretError)
import Hex.Stage.Lex.Interface.CharSource (CharSource, newCharSource)
import Hex.Stage.Lex.Interface.Extract qualified as Lex
import Hexlude

data HexStateWithChars = HexStateWithChars H.St.HexState CharSource
  deriving stock (Generic)

instance {-# OVERLAPPING #-} HasType ByteString HexStateWithChars where
  typed = typed @CharSource % typed @ByteString

newHexStateWithChars :: ByteString -> HexStateWithChars
newHexStateWithChars chrs = HexStateWithChars H.St.newHexState (newCharSource chrs)

newtype App a = App {unApp :: StateT HexStateWithChars (ExceptT AppError IO) a}
  deriving newtype (
    Functor, Applicative, Monad,
    MonadIO,
    MonadError AppError,
    MonadState HexStateWithChars
  )

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
