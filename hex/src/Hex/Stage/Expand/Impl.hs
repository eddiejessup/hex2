{-# LANGUAGE UndecidableInstances #-}
{-# OPTIONS_GHC -Wno-orphans #-}

module Hex.Stage.Expand.Impl where

import Hex.Common.HexState.Interface (MonadHexState (..))
import Hex.Common.HexState.Interface.Resolve (ResolvedToken (..))
import Hex.Common.HexState.Interface.Resolve.PrimitiveToken (PrimitiveToken)
import Hex.Common.HexState.Interface.Resolve.SyntaxToken qualified as Syn
import Hex.Stage.Expand.Interface (MonadPrimTokenSource (..))
import Hex.Stage.Lex.Interface qualified as Lex
import Hex.Stage.Lex.Interface.Extract (LexToken)
import Hex.Stage.Lex.Interface.Extract qualified as Lex
import Hex.Stage.Resolve.Interface (ResolutionMode (..))
import Hex.Stage.Resolve.Interface qualified as Res
import Hexlude

data ExpansionError
  = ResolutionExpansionError Res.ResolutionError
  deriving stock (Generic, Show)

instance (Res.MonadResolvedTokenSource m, MonadError e m, AsType ExpansionError e, MonadHexState m) => MonadPrimTokenSource m where
  getPrimitiveToken = fetchPrimitiveToken

  getLastPrimitiveToken = getLastFetchedPrimTok

-- Note that the lex-tokens and resolved-tokens are just returned for debugging really.
-- They are passed through unchanged from the resolved-token-source.
fetchPrimitiveToken ::
  (Res.MonadResolvedTokenSource m, MonadError e m, AsType ExpansionError e, MonadHexState m) =>
  m (Maybe (LexToken, ResolvedToken, PrimitiveToken))
fetchPrimitiveToken = do
  -- Get the next lex-token from the input, and resolve it.
  Res.getMayResolvedToken Resolving >>= \case
    -- If nothing left in the input, return nothing.
    Nothing -> pure Nothing
    -- If we get a token, we only care about the resolved version.
    -- Check if resolution succeeded.
    Just (lt, errOrResolvedTok) -> case errOrResolvedTok of
      -- If resolution failed, throw an error.
      Left e ->
        throwError $ injectTyped $ ResolutionExpansionError e
      -- Otherwise, look at the result of resolution.
      -- Is it a primitive token, or does it need expansion?
      Right rt ->
        case rt of
          -- If it's a primitive token, we are done, just return that.
          PrimitiveToken pt -> do
            setLastFetchedPrimTok pt
            pure $ Just (lt, rt, pt)
          -- Otherwise, the token is the head of a syntax-command.
          SyntaxCommandHeadToken headTok -> do
            -- Expand the rest of the command into lex-tokens.
            lts <- expandSyntaxCommand headTok
            -- Insert those resulting lex-tokens back into the input.
            -- (It's not this function's concern,
            -- but recall they will be put on the lex-token-buffer).
            Lex.insertLexTokensToSource lts
            -- Try to read a primitive token again.
            -- Note that the new lex tokens might
            -- themselves introduce a syntax command,
            -- so we might need to expand again.
            fetchPrimitiveToken

expandSyntaxCommand ::
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
