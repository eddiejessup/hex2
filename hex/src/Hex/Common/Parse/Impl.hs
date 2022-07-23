{-# LANGUAGE UndecidableInstances #-}

module Hex.Common.Parse.Impl where

import Effectful.Dispatch.Dynamic (localSeqUnlift)
import Formatting qualified as F
import Hex.Capability.Log.Interface (HexLog)
import Hex.Common.HexInput.Interface qualified as HIn
import Hex.Common.Parse.Interface (PrimTokenParse (..))
import Hex.Common.Parse.Interface qualified as CPar
import Hex.Common.Token.Lexed qualified as LT
import Hex.Common.Token.Resolved.Primitive qualified as PT
import Hex.Stage.Expand.Interface (PrimTokenSource)
import Hex.Stage.Expand.Interface qualified as Expand
import Hexlude

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

runAlt :: Error ParsingError :> es => Eff (EAlternative : es) a -> Eff es a
runAlt = interpret $ \env -> \case
  AltEmpty -> throwError @ParsingError $ UnexpectedParsingError CPar.ParseDefaultFailure
  AltChoice a b -> do
    localSeqUnlift env $ \unlift -> do
      catchError @ParsingError (unlift a) $ \_callStack _e -> unlift b

-- instance ([PrimTokenParse, Error ParsingError] :>> es) => Alternative (Eff es) where
--   empty = throwError @ParsingError $ UnexpectedParsingError CPar.ParseDefaultFailure

--   (<|>) :: Eff es a -> Eff es a -> Eff es a
--   a <|> b = do
--     catchError @ParsingError a $ \_ _ -> b

-- instance (HexLog :> es) => MonadPlus (ParseT m)

-- parseErrorEndOfInput :: PrimTokenParse :> es => Eff es a
-- parseErrorEndOfInput = parseErrorImpl EndOfInputParsingError

-- recordLexToken :: PrimTokenParse :> es => LT.LexToken -> Eff es ()
-- recordLexToken lt = parseTAdd $ ParseLog $ Seq.singleton lt

satisfyThenCommon ::
  (HIn.HexInput :> es, Error ParsingError :> es) =>
  Eff es (Maybe (LT.LexToken, b)) ->
  ((LT.LexToken, b) -> Maybe a) ->
  Eff es a
satisfyThenCommon parser f = do
  parser >>= \case
    Nothing ->
      throwError @ParsingError $ UnexpectedParsingError $ CPar.ParseExplicitFailure "satisfyThen, no next primitive token"
    Just x@(lt, _) -> do
      case f x of
        Nothing -> do
          HIn.insertLexToken lt
          throwError @ParsingError $ UnexpectedParsingError $ CPar.ParseExplicitFailure $ "satisfyThen, test failed on lex-token: " <> F.sformat LT.fmtLexToken lt
        Just a -> do
          pure a

satisfyThenExpandingImpl ::
  (PrimTokenSource :> es, HIn.HexInput :> es, Error ParsingError :> es) =>
  ((LT.LexToken, PT.PrimitiveToken) -> Maybe a) ->
  Eff es a
satisfyThenExpandingImpl f =
  satisfyThenCommon
    Expand.getPrimitiveToken
    f

satisfyThenInhibitedImpl ::
  (PrimTokenSource :> es, HIn.HexInput :> es, Error ParsingError :> es) =>
  (LT.LexToken -> Maybe a) ->
  Eff es a
satisfyThenInhibitedImpl f =
  -- Wrap and unwrap using the trivial tuple to make a common interface we can share with the 'expanding' version.
  satisfyThenCommon
    (Expand.getTokenInhibited <&> (fmap (,())))
    (\(lt, ()) -> f lt)

runPrimTokenParse :: [HIn.HexInput, HexLog, PrimTokenSource, Error ParsingError] :>> es => Eff (PrimTokenParse : es) a -> Eff es a
runPrimTokenParse = interpret $ \env -> \case
  SatisfyThenExpanding x -> satisfyThenExpandingImpl x
  SatisfyThenInhibited x -> satisfyThenInhibitedImpl x
  TryParse parser -> do
    st <- HIn.getInput
    localSeqUnlift env $ \unlift -> do
      catchError @ParsingError (unlift parser) $ \_callStack e -> do
        HIn.putInput st
        throwError e
  FailParse e ->
    throwError $ UnexpectedParsingError e

runAltPrimTokenParse :: [HIn.HexInput, HexLog, PrimTokenSource, Error ParsingError] :>> es => Eff (PrimTokenParse : EAlternative : es) a -> Eff es a
runAltPrimTokenParse = runAlt . runPrimTokenParse

runAltPrimTokenParseMaybe :: [HIn.HexInput, HexLog, PrimTokenSource, Error CPar.ParseUnexpectedError] :>> es => Eff (PrimTokenParse : EAlternative : Error ParsingError : es) a -> Eff es (Maybe a)
runAltPrimTokenParseMaybe ef = do
  errOrRes <- runErrorNoCallStack @ParsingError (runAltPrimTokenParse ef)
  case errOrRes of
    Left EndOfInputParsingError -> pure Nothing
    Left (UnexpectedParsingError e) -> throwError e
    Right v -> pure $ Just v
