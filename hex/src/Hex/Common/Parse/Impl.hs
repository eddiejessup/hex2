module Hex.Common.Parse.Impl where

import Control.Monad.Trans (MonadTrans (..))
import Control.Monad.Trans.Writer.CPS qualified as W
import Data.Sequence qualified as Seq
import Formatting qualified as F
import Hex.Common.HexState.Interface (MonadHexState (..))
import Hex.Common.HexState.Interface.Resolve.PrimitiveToken qualified as PT
import Hex.Common.Parse.Interface
import Hex.Stage.Expand.Interface (MonadPrimTokenSource (..))
import Hex.Stage.Lex.Interface qualified as Lex
import Hex.Stage.Lex.Interface.Extract qualified as Lex
import Hexlude

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
newtype ParseT m a = ParseT {unParseT :: ExceptT ParsingError (W.WriterT ParseLog m) a}
  deriving newtype (Functor, Applicative, Monad, MonadError ParsingError)

mkParseT :: Monad m => m (Either ParsingError a, ParseLog) -> ParseT m a
mkParseT ma = ParseT $ ExceptT $ W.writerT ma

liftWriter :: Monad m => W.WriterT ParseLog m a -> ParseT m a
liftWriter ma = ParseT $ lift ma

runParseT :: ParseT m a -> m (Either ParsingError a, ParseLog)
runParseT = W.runWriterT . runExceptT . unParseT

-- From the perspective of the parser, ie in a MonadPrimTokenParse context,
-- we need 'end-of-input' to be an error like any other, so we can make a monoid
-- of the objects to combine parsers together, for 'alternative' behaviour.
-- But once we're done with parsing, we might want to treat end-of-input differently,
-- by returning a 'Nothing', as we might want to behave differently
-- instead of just failing in this case.
-- This helper lets us do this.
runParseTMaybe :: (MonadError e m, AsType ParseUnexpectedError e) => ParseT m a -> m (Maybe a, ParseLog)
runParseTMaybe p = do
  (errOrA, pLog) <- runParseT p
  case errOrA of
    Left EndOfInputParsingError -> pure (Nothing, pLog)
    Left (UnexpectedParsingError e) -> throwError $ injectTyped e
    Right a -> pure $ (Just a, pLog)

-- Run a parser, but backtrack the char-source if the parse fails.
-- (This is why we need the MonadLexTokenSource instance:
-- it lets us get at the underlying stream.)
runParseTWithTry :: Lex.MonadLexTokenSource m => ParseT m a -> m (Either ParsingError a, ParseLog)
runParseTWithTry a = do
  -- Get the initial state.
  src0 <- Lex.getSource
  -- Run the parser.
  (errOrV, pLog) <- runParseT a
  -- If we got a parse error, reset the state to the initial state.
  case errOrV of
    Left _ -> do
      Lex.putSource src0
    _ ->
      pure ()
  -- Either way, still return the result, error or not.
  pure (errOrV, pLog)

instance MonadTrans ParseT where
  lift = ParseT . lift . lift

-- I can implement alternative on ParseT m, if we have MonadLexTokenSource m.
-- This is because we need to be able to get the source, to reset to that state if
-- our first element fails.
-- For this get-the-source function, we will use the methods in MonadLexTokenSource.
-- We could make this get-the-source ability abstract, but let's keep it concrete, and
-- therefore specialised to the particular Hex case.
-- (We are implementing a backtracking parser.)
instance (Lex.MonadLexTokenSource m, MonadHexState m) => Alternative (ParseT m) where
  empty = parseErrorImpl ParseDefaultFailure

  (<|>) :: ParseT m a -> ParseT m a -> ParseT m a
  a <|> b = do
    -- Run the first parser. but not 'in' the ParseT monad,
    -- run it explicitly so we get the 'Either' out.
    (errOrV, pLog) <- lift $ runParseTWithTry a
    -- If the parse fails, do the same for the second parser.
    case errOrV of
      Left _ -> do
        -- This should actually run in the ParseT monad,
        -- because failure here does mean failure of the monad.
        mkParseT $ runParseTWithTry b
      Right v -> do
        liftWriter $ W.tell pLog
        pure v

instance (Lex.MonadLexTokenSource m, MonadHexState m) => MonadPlus (ParseT m)

parseErrorEndOfInput :: Monad m => ParseT m a
parseErrorEndOfInput = ParseT $ throwE EndOfInputParsingError

-- The most common case, applies to all failures except end-of-input.
parseErrorImpl :: (MonadHexState m) => ParseUnexpectedErrorCause -> ParseT m a
parseErrorImpl e = do
  ParseT $ throwE $ UnexpectedParsingError $ ParseUnexpectedError e

-- - We need MonadLexTokenSource because it lets us get at the underlying char-source, for resetting state.
-- - We need MonadPrimTokenSource to get primitive-tokens to inspect.
satisfyThenImpl :: (Lex.MonadLexTokenSource m, MonadPrimTokenSource m, MonadHexState m) => (PT.PrimitiveToken -> Maybe a) -> ParseT m a
satisfyThenImpl f = do
  src0 <- lift Lex.getSource
  -- Fetch the new primitive-token.
  -- This might cause some expansion, i.e. some stateful changes to the input.
  pt <- snd <$> getExpandedTokenErrImpl
  -- Apply our quasi-predicate to the next primitive-token.
  case f pt of
    -- If our predicate fails, reset the source to its original state, i.e. before any expansion happens,
    -- and declare that we failed.
    Nothing -> do
      lift $ Lex.putSource src0
      parseErrorImpl $ SawUnexpectedPrimitiveToken (UnexpectedPrimitiveToken {saw = pt, expected = "Unknown"})
    -- If our predicate succeeds, then return the value.
    Just v ->
      pure v

-- Take a program that returns a 'Maybe a', with `Nothing` representing end-of-input, and
-- map that `Nothing` into an end-of-input error.
endOfInputToError :: Monad m => m (Maybe a) -> ParseT m a
endOfInputToError prog =
  nothingToError (lift prog) EndOfInputParsingError

logLexToken :: Monad m => Lex.LexToken -> ParseT m ()
logLexToken lt = liftWriter $ W.tell $ ParseLog $ Seq.singleton lt

-- Like `getExpandedTokenImpl`, but on end-of-input raise an error so the parse fails.
getExpandedTokenErrImpl :: (MonadHexState m, MonadPrimTokenSource m) => ParseT m (Lex.LexToken, PT.PrimitiveToken)
getExpandedTokenErrImpl = do
  r@(lt, _) <- endOfInputToError getPrimitiveToken
  logLexToken lt
  pure r

getUnexpandedTokenImpl :: (MonadHexState m, MonadPrimTokenSource m) => ParseT m Lex.LexToken
getUnexpandedTokenImpl = do
  lt <- endOfInputToError getTokenInhibited
  logLexToken lt
  pure lt

-- I want to write my parsers in 'MonadPrimTokenParse m => m'.
-- I could write them in 'Lex.MonadLexTokenSource m, MonadPrimTokenSource m => ParseT m',
-- but this would be a lot of typing for each parser.
-- So instead, I will implement 'MonadPrimTokenParse' for 'ParseT m'.

instance (Lex.MonadLexTokenSource m, MonadPrimTokenSource m, MonadHexState m) => MonadPrimTokenParse (ParseT m) where
  getExpandedToken = getExpandedTokenErrImpl

  getUnexpandedToken = getUnexpandedTokenImpl

  satisfyThen = satisfyThenImpl

  parseError = parseErrorImpl
