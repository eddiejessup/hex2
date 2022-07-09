{-# LANGUAGE UndecidableInstances #-}

module Hex.Common.Parse.Impl where

import Control.Monad.Trans (MonadTrans (..))
import Control.Monad.Trans.Writer.CPS qualified as W
import Data.Sequence qualified as Seq
import Formatting qualified as F
import Hex.Capability.Log.Interface qualified as Log
import Hex.Common.HexState.Interface.Resolve.PrimitiveToken qualified as PT
import Hex.Common.Parse.Interface (MonadPrimTokenParse)
import Hex.Common.Parse.Interface qualified as CPar
import Hex.Stage.Expand.Interface qualified as Expand
import Hex.Stage.Lex.Interface qualified as Lex
import Hex.Stage.Lex.Interface.Extract qualified as Lex
import Hexlude
import Hex.Stage.Expand.Interface (MonadPrimTokenSource)

newtype ParseLog = ParseLog {unParseLog :: Seq Lex.LexToken}
  deriving stock (Show)
  deriving newtype (Semigroup, Monoid)

fmtParseLog :: Fmt ParseLog
fmtParseLog = F.accessed unParseLog fmtLexTokenSeq
  where
    fmtLexTokenSeq :: Fmt (Seq Lex.LexToken)
    fmtLexTokenSeq = F.concatenated Lex.fmtLexTokenChar

-- This is the monad we will do our parsing in.
-- The main thing a parser does is implement choice: we can do `a <|> b`,
-- and if `a` fails we should try `b`, and the overall expression shouldn't fail.
-- That is why we have the extra `ExceptT ParsingError` layer.
newtype ParseT m a = ParseT {unParseT :: ExceptT CPar.ParsingError (W.WriterT ParseLog m) a}
  deriving newtype (Functor, Applicative, Monad, MonadError CPar.ParsingError)

instance Log.MonadHexLog m => Log.MonadHexLog (ParseT m) where
  log x = lift $ Log.log x

  logInternalState = lift Log.logInternalState

instance Expand.MonadPrimTokenSource m => Expand.MonadPrimTokenSource (ParseT m) where
  getTokenInhibited = lift Expand.getTokenInhibited

  getResolvedToken = lift Expand.getResolvedToken

  getPrimitiveToken = lift Expand.getPrimitiveToken

  pushConditionState x = lift $ Expand.pushConditionState x

  popConditionState = lift Expand.popConditionState

  peekConditionState = lift Expand.peekConditionState

instance Lex.MonadLexTokenSource m => Lex.MonadLexTokenSource (ParseT m) where
  getLexToken = lift Lex.getLexToken

  insertLexTokenToSource x = lift $ Lex.insertLexTokenToSource x

  insertLexTokensToSource x = lift $ Lex.insertLexTokensToSource x

  getSource = lift Lex.getSource

  putSource x = lift $ Lex.putSource x

mkParseT :: Monad m => m (Either CPar.ParsingError a, ParseLog) -> ParseT m a
mkParseT ma = ParseT $ ExceptT $ W.writerT ma

liftWriter :: Monad m => W.WriterT ParseLog m a -> ParseT m a
liftWriter ma = ParseT $ lift ma

runParseT :: ParseT m a -> m (Either CPar.ParsingError a, ParseLog)
runParseT = W.runWriterT . runExceptT . unParseT

-- From the perspective of the parser, ie in a MonadPrimTokenParse context,
-- we need 'end-of-input' to be an error like any other, so we can make a monoid
-- of the objects to combine parsers together, for 'alternative' behaviour.
-- But once we're done with parsing, we might want to treat end-of-input differently,
-- by returning a 'Nothing', as we might want to behave differently
-- instead of just failing in this case.
-- This helper lets us do this.
runParseTMaybe :: (MonadError e m, AsType CPar.ParseUnexpectedError e) => ParseT m a -> m (Maybe a, ParseLog)
runParseTMaybe p = do
  (errOrA, pLog) <- runParseT p
  case errOrA of
    Left CPar.EndOfInputParsingError -> pure (Nothing, pLog)
    Left (CPar.UnexpectedParsingError e) -> throwError $ injectTyped e
    Right a -> pure $ (Just a, pLog)

instance MonadTrans ParseT where
  lift = ParseT . lift . lift

-- I can implement alternative on ParseT m, if we have MonadLexTokenSource m.
-- This is because we need to be able to get the source, to reset to that state if
-- our first element fails.
-- For this get-the-source function, we will use the methods in MonadLexTokenSource.
-- We could make this get-the-source ability abstract, but let's keep it concrete, and
-- therefore specialised to the particular Hex case.
-- (We are implementing a backtracking parser.)
instance (Monad m, Lex.MonadLexTokenSource (ParseT m)) => Alternative (ParseT m) where
  empty = parseErrorImpl CPar.ParseDefaultFailure

  (<|>) :: ParseT m a -> ParseT m a -> ParseT m a
  a <|> b = do
    -- Run the first parser. but not 'in' the ParseT monad,
    -- run it explicitly so we get the 'Either' out.
    (errOrV, pLog) <- lift $ runParseT a
    -- If the parse fails, do the same for the second parser.
    case errOrV of
      Left _ ->
        -- This should actually run in the ParseT monad,
        -- because failure here does mean failure of the monad.
        b
      Right v -> do
        liftWriter $ W.tell pLog
        pure v

instance (Lex.MonadLexTokenSource (ParseT m), Monad m) => MonadPlus (ParseT m)

parseErrorEndOfInput :: Monad m => ParseT m a
parseErrorEndOfInput = ParseT $ throwE CPar.EndOfInputParsingError

-- The most common case, applies to all failures except end-of-input.
parseErrorImpl :: (Monad m) => CPar.ParseUnexpectedErrorCause -> ParseT m a
parseErrorImpl e = do
  ParseT $ throwE $ CPar.UnexpectedParsingError $ CPar.ParseUnexpectedError e

-- Take a program that returns a 'Maybe a', with `Nothing` representing end-of-input, and
-- map that `Nothing` into an end-of-input error.
endOfInputToError :: Monad m => ParseT m (Maybe a) -> ParseT m a
endOfInputToError prog =
  nothingToError prog CPar.EndOfInputParsingError

recordLexToken :: Monad m => Lex.LexToken -> ParseT m ()
recordLexToken lt = liftWriter $ W.tell $ ParseLog $ Seq.singleton lt

-- Like `getExpandedTokenImpl`, but on end-of-input raise an error so the parse fails.
getExpandedTokenErrImpl :: (Monad m, MonadPrimTokenSource (ParseT m), Log.MonadHexLog (ParseT m)) => ParseT m (Lex.LexToken, PT.PrimitiveToken)
getExpandedTokenErrImpl = do
  Log.log $ "Parser: Getting primitive-token"
  r@(lt, _) <- endOfInputToError Expand.getPrimitiveToken
  recordLexToken lt
  pure r

getUnexpandedTokenImpl :: (Monad m, MonadPrimTokenSource (ParseT m), Log.MonadHexLog (ParseT m)) => ParseT m Lex.LexToken
getUnexpandedTokenImpl = do
  Log.log $ "Parser: Getting lex-token"
  lt <- endOfInputToError Expand.getTokenInhibited
  recordLexToken lt
  Log.log $ "Parser: Got unexpanded lex-token: " <> show lt
  pure lt

satisfyThenExpandingImpl ::
  (Monad m, MonadPrimTokenSource (ParseT m), Lex.MonadLexTokenSource (ParseT m), Log.MonadHexLog (ParseT m)) =>
  ((Lex.LexToken, PT.PrimitiveToken) -> Maybe a) ->
  ParseT m a
satisfyThenExpandingImpl f = do
  Expand.getPrimitiveToken >>= \case
    Nothing ->
      CPar.parseFailure "satisfyThenExpandingImpl, no next primitive token"
    Just x@(lt, pt) -> do
      case f x of
        Nothing -> do
          Lex.insertLexTokenToSource lt
          CPar.parseFailure $ "satisfyThenExpandingImpl, test failed for primitive-token: " <> F.sformat PT.fmtPrimitiveToken pt
        Just a -> do
          Log.log $ "Committing token while expanding: " <> F.sformat Lex.fmtLexToken lt
          recordLexToken lt
          pure a

satisfyThenInhibitedImpl ::
  (Monad m, MonadPrimTokenSource (ParseT m), Lex.MonadLexTokenSource (ParseT m), Log.MonadHexLog (ParseT m)) =>
  (Lex.LexToken -> Maybe a) ->
  ParseT m a
satisfyThenInhibitedImpl f = do
  Expand.getTokenInhibited >>= \case
    Nothing -> do
      CPar.parseFailure "satisfyThenInhibitedImpl, no next lex token"
    Just lt -> do
      case f lt of
        Nothing -> do
          Lex.insertLexTokenToSource lt
          CPar.parseFailure $ "satisfyThenInhibitedImpl, test failed for lex-token: " <> F.sformat Lex.fmtLexToken lt
        Just a -> do
          Log.log $ "Committing token while inhibited: " <> F.sformat Lex.fmtLexToken lt
          recordLexToken lt
          pure a

instance (Monad m, Lex.MonadLexTokenSource (ParseT m), MonadPrimTokenSource (ParseT m), Log.MonadHexLog (ParseT m)) => MonadPrimTokenParse (ParseT m) where
  -- getExpandedToken = getExpandedTokenErrImpl

  -- getUnexpandedToken = getUnexpandedTokenImpl

  parseError = parseErrorImpl

  satisfyThenExpanding = satisfyThenExpandingImpl

  satisfyThenInhibited = satisfyThenInhibitedImpl
