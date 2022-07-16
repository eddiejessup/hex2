{-# LANGUAGE UndecidableInstances #-}

module Hex.Common.Parse.Impl where

import Control.Monad.Trans (MonadTrans (..))
import Control.Monad.Trans.Accum qualified as Accum
import Data.Sequence qualified as Seq
import Formatting qualified as F
import Hex.Capability.Log.Interface qualified as Log
import Hex.Common.HexInput.Interface qualified as HIn
import Hex.Common.Parse.Interface (MonadPrimTokenParse)
import Hex.Common.Parse.Interface qualified as CPar
import Hex.Common.Token.Lexed qualified as LT
import Hex.Common.Token.Resolved.Primitive qualified as PT
import Hex.Stage.Expand.Interface (MonadPrimTokenSource)
import Hex.Stage.Expand.Interface qualified as Expand
import Hexlude

-- This is the monad we will do our parsing in.
-- The main thing a parser does is implement choice: we can do `a <|> b`,
-- and if `a` fails we should try `b`, and the overall expression shouldn't fail.
-- That is why we have the extra `ExceptT ParsingError` layer.
newtype ParseT m a = ParseT {unParseT :: ExceptT ParsingErrorWithContext (Accum.AccumT ParseLog m) a}
  deriving newtype (Functor, Applicative, Monad, MonadError ParsingErrorWithContext)

data ParsingErrorWithContext = ParsingErrorWithContext {parsingError :: ParsingError, parseContext :: ParseLog}
  deriving stock (Show, Eq, Generic)

newtype ParseLog = ParseLog {unParseLog :: Seq LT.LexToken}
  deriving stock (Show, Eq)
  deriving newtype (Semigroup, Monoid)

fmtParseLog :: Fmt ParseLog
fmtParseLog = F.accessed unParseLog fmtLexTokenSeq
  where
    fmtLexTokenSeq :: Fmt (Seq LT.LexToken)
    fmtLexTokenSeq = F.concatenated LT.fmtLexTokenChar

fmtParsingErrorWithContext :: Fmt ParsingErrorWithContext
fmtParsingErrorWithContext = ("Parsing error: " |%| F.accessed (.parsingError) fmtParsingError) <> (", parse context: " |%| F.accessed (.parseContext) fmtParseLog)

data ParsingError
  = EndOfInputParsingError
  | UnexpectedParsingError CPar.ParseUnexpectedError
  deriving stock (Show, Eq, Generic)

fmtParsingError :: Fmt ParsingError
fmtParsingError = F.later $ \case
  EndOfInputParsingError -> "End of input"
  UnexpectedParsingError e -> F.bformat CPar.fmtParseUnexpectedErrorCause e

instance MonadTrans ParseT where
  lift = ParseT . lift . lift

instance Log.MonadHexLog m => Log.MonadHexLog (ParseT m) where
  log x y = lift $ Log.log x y

  logInternalState = lift Log.logInternalState

instance Expand.MonadPrimTokenSource m => Expand.MonadPrimTokenSource (ParseT m) where
  getTokenInhibited = lift Expand.getTokenInhibited

  getResolvedToken = lift Expand.getResolvedToken

  getPrimitiveToken = lift Expand.getPrimitiveToken

  pushConditionState x = lift $ Expand.pushConditionState x

  popConditionState = lift Expand.popConditionState

  peekConditionState = lift Expand.peekConditionState

instance HIn.MonadHexInput m => HIn.MonadHexInput (ParseT m) where
  endCurrentLine = lift HIn.endCurrentLine

  inputIsFinished = lift HIn.inputIsFinished

  getInput = lift HIn.getInput

  putInput = lift . HIn.putInput

  insertLexToken = lift . HIn.insertLexToken

  insertLexTokens = lift . HIn.insertLexTokens

  getNextLexToken = lift HIn.getNextLexToken

  openInputFile x = lift $ HIn.openInputFile x

resumeParseT :: Monad m => Either ParsingErrorWithContext a -> ParseLog -> ParseT m a
resumeParseT a parseLog = ParseT $ ExceptT $ Accum.accum $ \_w -> (a, parseLog)

mkParseT :: Monad m => ParseLog -> ParseT m ()
mkParseT parseLog = ParseT $ ExceptT $ Accum.AccumT $ \_w -> pure (Right (), parseLog)

-- >>> let x = resumeParseT (Right ()) (ParseLog (Seq.singleton Lex.spaceTok))
-- >>> let plog = x >> x >> parseTLook >> parseErrorEndOfInput
-- >>> fst (runIdentity $ runParseT @Identity plog)
-- Left (ParsingErrorWithContext {parsingError = EndOfInputParsingError, parseContext = ParseLog {unParseLog = fromList [CharCatLexToken (LexCharCat {lexCCChar = CharCode {unCharCode = 32}, lexCCCat = Space}),CharCatLexToken (LexCharCat {lexCCChar = CharCode {unCharCode = 32}, lexCCCat = Space}),CharCatLexToken (LexCharCat {lexCCChar = CharCode {unCharCode = 32}, lexCCCat = Space})]}})

-- >>> let x = mkParseT (ParseLog (Seq.singleton Lex.spaceTok))
-- >>> let plog = x >> x >> parseTLook >> parseErrorEndOfInput
-- >>> fst (runIdentity $ runParseT @Identity plog)
-- Left (ParsingErrorWithContext {parsingError = EndOfInputParsingError, parseContext = ParseLog {unParseLog = fromList [CharCatLexToken (LexCharCat {lexCCChar = CharCode {unCharCode = 32}, lexCCCat = Space}),CharCatLexToken (LexCharCat {lexCCChar = CharCode {unCharCode = 32}, lexCCCat = Space})]}})

liftAccumT :: Monad m => Accum.AccumT ParseLog m a -> ParseT m a
liftAccumT acc = ParseT $ lift $ acc

parseTAdd :: Monad m => ParseLog -> ParseT m ()
parseTAdd pLog = liftAccumT $ Accum.add pLog

parseTLook :: Monad m => ParseT m ParseLog
parseTLook = liftAccumT Accum.look

runParseT :: ParseT m a -> m (Either ParsingErrorWithContext a, ParseLog)
runParseT parseT = Accum.runAccumT (runExceptT $ unParseT parseT) mempty

-- From the perspective of the parser, ie in a MonadPrimTokenParse context,
-- we need 'end-of-input' to be an error like any other, so we can make a monoid
-- of the objects to combine parsers together, for 'alternative' behaviour.
-- But once we're done with parsing, we might want to treat end-of-input differently,
-- by returning a 'Nothing', as we might want to behave differently
-- instead of just failing in this case.
-- This helper lets us do this.
runParseTMaybe :: (MonadError e m, AsType ParsingErrorWithContext e, Log.MonadHexLog m) => ParseT m a -> m (Maybe a, ParseLog)
runParseTMaybe p = do
  (errOrA, pLog) <- runParseT p
  Log.debugLog $ "After running parseT in Maybe ctx, parse-log looks like: " <> F.sformat fmtParseLog pLog
  case errOrA of
    Left (ParsingErrorWithContext EndOfInputParsingError _) -> pure (Nothing, pLog)
    Left e@(ParsingErrorWithContext (UnexpectedParsingError _userError) _) -> throwError $ injectTyped e
    Right a -> pure $ (Just a, pLog)

-- I can implement alternative on ParseT m, if we have MonadLexTokenSource m.
-- This is because we need to be able to get the source, to reset to that state if
-- our first element fails.
-- For this get-the-source function, we will use the methods in MonadLexTokenSource.
-- We could make this get-the-source ability abstract, but let's keep it concrete, and
-- therefore specialised to the particular Hex case.
-- (We are implementing a backtracking parser.)
instance (Monad m, Log.MonadHexLog (ParseT m)) => Alternative (ParseT m) where
  empty = parseUserErrorImpl CPar.ParseDefaultFailure

  (<|>) :: ParseT m a -> ParseT m a -> ParseT m a
  a <|> b = do
    -- Run the first parser. but not 'in' the ParseT monad,
    -- run it explicitly so we get the 'Either' out.
    (errOrV, pLog) <- lift $ runParseT a

    -- If the parse fails, do the same for the second parser.
    case errOrV of
      Left _ -> do
        Log.debugLog $ "After running first option in alternative with failure, parse-log looks like: " <> F.sformat fmtParseLog pLog
        -- This should actually run in the ParseT monad,
        -- because failure here does mean failure of the monad.
        b
      Right v -> do
        Log.debugLog $ "After running first option in alternative with success, parse-log looks like: " <> F.sformat fmtParseLog pLog
        -- Add the log we got from the 'a' parser.
        parseTAdd pLog
        pure v

instance (Monad m, Log.MonadHexLog (ParseT m)) => MonadPlus (ParseT m)

parseErrorEndOfInput :: Monad m => ParseT m a
parseErrorEndOfInput = parseErrorImpl EndOfInputParsingError

-- The most common case, applies to all failures except end-of-input.
parseErrorImpl :: (Monad m) => ParsingError -> ParseT m a
parseErrorImpl e = do
  pLog <- parseTLook
  ParseT $ throwE $ ParsingErrorWithContext e pLog

-- The most common case, applies to all failures except end-of-input.
parseUserErrorImpl :: (Monad m) => CPar.ParseUnexpectedError -> ParseT m a
parseUserErrorImpl e = do
  parseErrorImpl $ UnexpectedParsingError e

recordLexToken :: Monad m => LT.LexToken -> ParseT m ()
recordLexToken lt = parseTAdd $ ParseLog $ Seq.singleton lt

tryImpl ::
  (Monad m, MonadPrimTokenParse (ParseT m), HIn.MonadHexInput (ParseT m)) =>
  ParseT m a ->
  ParseT m a
tryImpl f = do
  Log.debugLog "Doing try"
  st <- HIn.getInput
  (errOrA, pLog) <- lift $ runParseT f
  case errOrA of
    Left e -> do
      Log.debugLog $ "Try target failed with error: " <> F.sformat fmtParsingErrorWithContext e
      HIn.putInput st
    Right _ -> do
      Log.debugLog "Try target succeeded"
      pure ()
  -- Whether we succeeded or failed, package up the result as a ParseT.
  resumeParseT errOrA pLog

satisfyThenCommon ::
  (Monad m, MonadPrimTokenSource (ParseT m), HIn.MonadHexInput (ParseT m), Log.MonadHexLog (ParseT m)) =>
  ParseT m (Maybe (LT.LexToken, b)) ->
  ((LT.LexToken, b) -> Maybe a) ->
  ParseT m a
satisfyThenCommon parser f = do
  parser >>= \case
    Nothing ->
      CPar.parseFailure "satisfyThen, no next primitive token"
    Just x@(lt, _) -> do
      case f x of
        Nothing -> do
          HIn.insertLexToken lt
          CPar.parseFailure $ "satisfyThen, test failed on lex-token: " <> F.sformat LT.fmtLexToken lt
        Just a -> do
          recordLexToken lt
          Log.infoLog $ "Committing token: " <> F.sformat LT.fmtLexToken lt
          pure a

satisfyThenExpandingImpl ::
  (Monad m, MonadPrimTokenSource (ParseT m), HIn.MonadHexInput (ParseT m), Log.MonadHexLog (ParseT m)) =>
  ((LT.LexToken, PT.PrimitiveToken) -> Maybe a) ->
  ParseT m a
satisfyThenExpandingImpl f =
  satisfyThenCommon
    Expand.getPrimitiveToken
    f

satisfyThenInhibitedImpl ::
  (Monad m, MonadPrimTokenSource (ParseT m), HIn.MonadHexInput (ParseT m), Log.MonadHexLog (ParseT m)) =>
  (LT.LexToken -> Maybe a) ->
  ParseT m a
satisfyThenInhibitedImpl f =
  -- Wrap and unwrap using the trivial tuple to make a common interface we can share with the 'expanding' version.
  satisfyThenCommon
    (Expand.getTokenInhibited <&> (fmap (,())))
    (\(lt, ()) -> f lt)

instance (Monad m, HIn.MonadHexInput (ParseT m), MonadPrimTokenSource (ParseT m), Log.MonadHexLog (ParseT m)) => MonadPrimTokenParse (ParseT m) where
  parseError = parseUserErrorImpl

  satisfyThenExpanding = satisfyThenExpandingImpl

  satisfyThenInhibited = satisfyThenInhibitedImpl

  try = tryImpl
