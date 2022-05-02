-- In order to expand syntax-commands into primitive tokens,
-- we need to be able to parse primitive-token streams.
-- This might seem circular, and it is! But because syntax-commands
-- can need to be expanded recursively, this is needed.
module Hex.Stage.Expand.Impl.Parse where

import Control.Monad.Trans (MonadTrans (..))
import Hex.Common.HexState.Interface (MonadHexState (..))
import Hex.Common.HexState.Interface.Resolve.PrimitiveToken qualified as PT
import Hex.Common.Parse
import Hex.Stage.Expand.Interface (MonadPrimTokenSource (..), getPrimitiveToken)
import Hex.Stage.Lex.Interface qualified as Lex
import Hexlude
import qualified Hex.Stage.Resolve.Interface as Res
import qualified Hex.Stage.Lex.Interface.Extract as Lex

-- This is the monad we will do our parsing in.
-- The main thing a parser does is implement choice: we can do `a <|> b`,
-- and if `a` fails we should try `b`, and the overall expression shouldn't fail.
-- That is why we have the extra `ExceptT ParsingError` layer.
newtype ParseT m a = ParseT {unParseT :: ExceptT ParsingError m a}
  deriving newtype (Functor, Applicative, Monad, MonadError ParsingError)

mkParseT :: m (Either ParsingError a) -> ParseT m a
mkParseT = ParseT . ExceptT

runParseT :: ParseT m a -> m (Either ParsingError a)
runParseT = runExceptT . unParseT

-- Run a parser, but backtrack the char-source if the parse fails.
-- (This is why we need the MonadLexTokenSource instance:
-- it lets us get at the underlying stream.)
runParseTWithTry :: Lex.MonadLexTokenSource m => ParseT m a -> m (Either ParsingError a)
runParseTWithTry a = do
  -- Get the initial state.
  src0 <- Lex.getSource
  -- Run the parser.
  errOrV <- runParseT a
  -- If we got a parse error, reset the state to the initial state.
  case errOrV of
    Left _ -> do
      Lex.putSource src0
    _ ->
      pure ()
  -- Either way, still return the result, error or not.
  pure errOrV

instance MonadTrans ParseT where
  lift ma = mkParseT $ Right <$> ma

-- I can implement alternative on ParseT m, if we have MonadLexTokenSource m.
-- This is because we need to be able to get the source, to reset to that state if
-- our first element fails.
-- For this get-the-source function, we will use the methods in MonadLexTokenSource.
-- We could make this get-the-source ability abstract, but let's keep it concrete, and
-- therefore specialised to the particular Hex case.
-- (We are implementing a backtracking parser.)
instance (Lex.MonadLexTokenSource m, MonadHexState m) => Alternative (ParseT m) where
  empty = parseErrorImpl ParseExplicitFailure

  (<|>) :: ParseT m a -> ParseT m a -> ParseT m a
  a <|> b = do
    -- Run the first parser. but not 'in' the ParseT monad,
    -- run it explicitly so we get the 'Either' out.
    errOrV <- lift $ runParseTWithTry a
    -- If the parse fails, do the same for the second parser.
    case errOrV of
      Left _ -> do
        -- This should actually run in the ParseT monad,
        -- because failure here does mean failure of the monad.
        mkParseT $ runParseTWithTry b
      Right v ->
        pure v

instance (Lex.MonadLexTokenSource m, MonadHexState m) => MonadPlus (ParseT m)

parseErrorEndOfInput :: Monad m => ParseT m a
parseErrorEndOfInput = ParseT $ throwE EndOfInputParsingError

-- The most common case, applies to all failures except end-of-input.
parseErrorImpl :: (MonadHexState m) => ParseUnexpectedErrorCause -> ParseT m a
parseErrorImpl e = do
  lastLexTok <- lift getLastFetchedLexTok
  ParseT $ throwE $ UnexpectedParsingError $ ParseUnexpectedError lastLexTok e

-- - We need MonadLexTokenSource because it lets us get at the underlying char-source, for resetting state.
-- - We need MonadPrimTokenSource to get primitive-tokens to inspect.
satisfyThenImpl :: (Lex.MonadLexTokenSource m, MonadPrimTokenSource m, MonadHexState m) => (PT.PrimitiveToken -> Maybe a) -> ParseT m a
satisfyThenImpl f = do
  src0 <- lift Lex.getSource
  -- Fetch the new primitive-token.
  -- This might cause some expansion, i.e. some stateful changes to the input.
  pt <- getAnyPrimitiveTokenErrImpl
  -- Apply our quasi-predicate to the next primitive-token.
  case f pt of
    -- If our predicate fails, reset the source to its original state, i.e. before any expansion happens,
    -- and declare that we failed.
    Nothing -> do
      lift $ Lex.putSource src0
      parseErrorImpl $ SawUnexpectedToken (UnexpectedToken {saw = pt, expected = "Unknown"})
    -- If our predicate succeeds, then return the value.
    Just v ->
      pure v

getAnyPrimitiveTokenImpl :: (MonadHexState m, MonadPrimTokenSource m) => m (Maybe PT.PrimitiveToken)
getAnyPrimitiveTokenImpl =
  -- Find out whether we're currently resolving, and get a primitive-token according to that mode.
  -- Then take just the primitive token from the result.
  getResolutionMode >>= getPrimitiveToken <&> \case
    Nothing -> Nothing
    Just (_lt, pt) -> Just pt

-- Take a program that returns a 'Maybe a', with `Nothing` representing end-of-input, and
-- map that `Nothing` into an end-of-input error.
endOfInputToError :: Monad m => m (Maybe a) -> ParseT m a
endOfInputToError prog =
  nothingToError (lift prog) EndOfInputParsingError

-- Like `getAnyPrimitiveTokenImpl`, but on end-of-input raise an error so the parse fails.
getAnyPrimitiveTokenErrImpl :: (MonadHexState m, MonadPrimTokenSource m) => ParseT m PT.PrimitiveToken
getAnyPrimitiveTokenErrImpl = endOfInputToError getAnyPrimitiveTokenImpl

getAnyLexTokenImpl :: (MonadHexState m, MonadPrimTokenSource m) => ParseT m Lex.LexToken
getAnyLexTokenImpl =
  endOfInputToError getTokenNotResolving

-- I want to write my parsers in 'MonadPrimTokenParse m => m'.
-- I could write them in 'Lex.MonadLexTokenSource m, MonadPrimTokenSource m => ParseT m',
-- but this would be a lot of typing for each parser.
-- So instead, I will implement 'MonadPrimTokenParse' for 'ParseT m'.

instance (Lex.MonadLexTokenSource m, MonadPrimTokenSource m, MonadHexState m) => MonadPrimTokenParse (ParseT m) where
  getAnyPrimitiveToken = getAnyPrimitiveTokenErrImpl

  getAnyLexToken = getAnyLexTokenImpl

  satisfyThen = satisfyThenImpl

  withInhibition :: (InhibitionToken -> ParseT m a) -> ParseT m a
  withInhibition p = do
    -- Save previous resolution-mode.
    prevResMode <- lift getResolutionMode
    -- Set resolution-mode to 'not-resolving', i.e. inhibited.
    lift $ setResolutionMode Res.NotResolving
    -- Run the parser, providing the inhibition-token that proves we have stopped resolution.
    res <- p InhibitionToken
    -- Restore the resolution-mode to its previous state.
    lift $ setResolutionMode prevResMode
    pure res

  parseError = parseErrorImpl
