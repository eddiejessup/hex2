{-# LANGUAGE UndecidableInstances #-}
{-# OPTIONS_GHC -Wno-orphans #-}

module Hex.Stage.Expand.Impl where

import Control.Monad.Trans (MonadTrans (..))
import Hex.Common.HexState.Interface.Resolve (ResolvedToken (..))
import Hex.Common.HexState.Interface.Resolve.PrimitiveToken (PrimitiveToken)
import Hex.Common.HexState.Interface.Resolve.SyntaxToken qualified as Syn
import Hex.Stage.Expand.Interface (MonadPrimTokenSource (..), ParsingError (..))
import Hex.Stage.Lex.Interface.Extract qualified as Lex
import Hex.Stage.Resolve.Interface (ResolutionMode (..))
import Hex.Stage.Resolve.Interface qualified as Res
import Hexlude

data ExpansionError
  = UnexpectedEndOfInputExpansionError
  | ResolutionExpansionError Res.ResolutionError
  deriving stock (Generic, Show)

newtype ParseT m a = ParseT {unParseT :: ExceptT ParsingError m a}
  deriving newtype (Functor, Applicative, Monad)

instance MonadTrans ParseT where
  lift ma = ParseT $ ExceptT $ Right <$> ma

instance Res.MonadResolvedTokenSource m => Alternative (ParseT m) where
  empty = ParseT $ throwE ParseFailure

  (<|>) :: ParseT m a -> ParseT m a -> ParseT m a
  a <|> b = do
    errOrV <- lift $ runExceptT (unParseT (parseTry a))
    case errOrV of
      Left _ -> do
        parseTry b
      Right v ->
        pure v

parseTry :: forall m a. (Monad m, Res.MonadResolvedTokenSource m) => ParseT m a -> ParseT m a
parseTry a = ParseT $ ExceptT go
  where
    go :: m (Either ParsingError a)
    go = do
      src0 <- Res.getSource
      errOrV <- runExceptT (unParseT a)
      case errOrV of
        Left _ -> do
          Res.putSource src0
        _ ->
          pure ()
      pure errOrV

instance (Res.MonadResolvedTokenSource m, MonadError e m, AsType ExpansionError e) => MonadPrimTokenSource (ParseT m) where
  fetchPT = lift fetchPrimitiveToken

  satisfyThen :: (PrimitiveToken -> Maybe a) -> ParseT m a
  satisfyThen f = do
    src0 <- lift Res.getSource
    t <- fetchPT
    case f t of
      Nothing -> do
        lift $ Res.putSource src0
        empty
      Just v ->
        pure v

  parseError e = ParseT $ throwE e

fetchPrimitiveToken :: (Res.MonadResolvedTokenSource m, MonadError e m, AsType ExpansionError e) => m PrimitiveToken
fetchPrimitiveToken = do
  Res.getLexToken >>= \case
    Nothing -> throwError $ injectTyped UnexpectedEndOfInputExpansionError
    Just lt ->
      Res.resolveLexToken Resolving lt >>= \case
        Left e ->
          throwError $ injectTyped $ ResolutionExpansionError e
        Right rt ->
          case rt of
            PrimitiveToken pt ->
              pure pt
            SyntaxCommandHeadToken headTok -> do
              lts <- expandSyntaxCommand headTok
              Res.insertLexTokensToSource lts
              fetchPrimitiveToken

instance Res.MonadResolvedTokenSource m => MonadPlus (ParseT m)

expandSyntaxCommand ::
  ( Res.MonadResolvedTokenSource m
  ) =>
  Syn.SyntaxCommandHeadToken ->
  m (Seq Lex.LexToken)
expandSyntaxCommand = \case {}

-- MacroTok m -> do
--   args <- parseMacroArgs m
--   expandMacro m args
-- ConditionTok ct -> do
--   expandConditionToken ct
--   pure mempty
-- NumberTok ->
--   panic "Not implemented: syntax command NumberTok"
-- RomanNumeralTok ->
--   panic "Not implemented: syntax command RomanNumeralTok"
-- StringTok -> do
--   conf <- use $ typed @Conf.Config
--   let escapeChar = (Conf.IntParamVal . Conf.lookupIntParameter EscapeChar) conf
--   expandString escapeChar <$> parseLexToken
-- JobNameTok ->
--   panic "Not implemented: syntax command JobNameTok"
-- FontNameTok ->
--   panic "Not implemented: syntax command FontNameTok"
-- MeaningTok ->
--   panic "Not implemented: syntax command MeaningTok"
-- CSNameTok -> do
--   a <- parseCSNameArgs
--   singleton <$> expandCSName a
-- ExpandAfterTok -> do
--   argLT <- takeLexToken
--   (_, postArgLTs) <- takeAndExpandResolvedToken
--   -- Prepend the unexpanded token.
--   pure (argLT <| postArgLTs)
-- NoExpandTok ->
--   panic "Not implemented: syntax command NoExpandTok"
-- MarkRegisterTok _ ->
--   panic "Not implemented: syntax command MarkRegisterTok"
-- -- \input ⟨file name⟩:
-- -- - Expand to no tokens
-- -- - Prepare to read from the specified file before looking at any more
-- --   tokens from the current source.
-- InputTok -> do
--   TeXFilePath texPath <- parseFileName
--   inputPath texPath
--   pure mempty
-- EndInputTok ->
--   panic "Not implemented: syntax command EndInputTok"
-- TheTok -> do
--   intQuant <- parseInternalQuantity
--   fmap charCodeAsMadeToken <$> texEvaluate intQuant
-- ChangeCaseTok direction -> do
--   conf <- use $ typed @Conf.Config
--   expandChangeCase
--     (\c -> Conf.lookupChangeCaseCode direction c conf)
--     <$> parseGeneralText
