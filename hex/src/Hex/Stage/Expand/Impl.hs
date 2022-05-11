{-# LANGUAGE UndecidableInstances #-}

module Hex.Stage.Expand.Impl where

import Hex.Capability.Log.Interface (MonadHexLog)
import Hex.Common.HexState.Interface qualified as HSt
import Hex.Common.HexState.Interface.Resolve (ResolvedToken (..))
import Hex.Common.HexState.Interface.Resolve.PrimitiveToken (PrimitiveToken)
import Hex.Common.HexState.Interface.Resolve.SyntaxToken qualified as ST
import Hex.Common.Parse qualified as Par
import Hex.Stage.Expand.Impl.Expand qualified as Expand
import Hex.Stage.Expand.Impl.Parse qualified as Par
import Hex.Stage.Expand.Impl.Parsers.SyntaxCommand.MacroCall qualified as Par
import Hex.Stage.Expand.Interface (ExpansionError (..), MonadPrimTokenSource (..))
import Hex.Stage.Lex.Interface qualified as Lex
import Hex.Stage.Lex.Interface.Extract (LexToken)
import Hex.Stage.Lex.Interface.Extract qualified as Lex
import Hex.Stage.Resolve.Interface qualified as Res
import Hexlude

newtype MonadPrimTokenSourceT m a = MonadPrimTokenSourceT {unMonadPrimTokenSourceT :: m a}
  deriving newtype
    ( Functor,
      Applicative,
      Monad,
      Alternative,
      MonadPlus,
      MonadIO,
      MonadState st,
      MonadError e,
      Res.MonadResolve,
      Lex.MonadLexTokenSource,
      Par.MonadPrimTokenParse,
      HSt.MonadHexState,
      MonadHexLog
    )

instance
  ( Res.MonadResolve (MonadPrimTokenSourceT m),
    MonadError e (MonadPrimTokenSourceT m),
    AsType ExpansionError e,
    Lex.MonadLexTokenSource (MonadPrimTokenSourceT m),
    HSt.MonadHexState (MonadPrimTokenSourceT m)
  ) =>
  MonadPrimTokenSource (MonadPrimTokenSourceT m)
  where
  getPrimitiveToken = getPrimitiveTokenImpl

  getTokenInhibited = Lex.getLexToken

-- Get the next lex-token from the input, resolve it, and expand it if
-- necessary.
-- Note that the lex-token is just returned for debugging really.
-- It is passed through unchanged from the lex-token-source.
getPrimitiveTokenImpl ::
  ( Res.MonadResolve m,
    MonadError e m,
    AsType ExpansionError e,
    Lex.MonadLexTokenSource m,
    MonadPrimTokenSource m,
    HSt.MonadHexState m
  ) =>
  m (Maybe (LexToken, PrimitiveToken))
getPrimitiveTokenImpl =
  Res.getMayResolvedToken >>= \case
    -- If nothing left in the input, return nothing.
    Nothing -> pure Nothing
    -- If we get a token, we only care about the resolved version.
    -- Check if resolution succeeded.
    Just (lt, errOrResolvedTok) -> case errOrResolvedTok of
      -- If resolution failed, throw an error.
      Left e ->
        throwError $ injectTyped $ ExpansionResolutionError e
      -- If we resolved to a primitive token, we are done, just return that.
      Right (PrimitiveToken pt) ->
        pure $ Just (lt, pt)
      -- Otherwise, the token is the head of a syntax-command.
      Right (SyntaxCommandHeadToken headTok) -> do
        -- Expand the rest of the command into lex-tokens.
        lts <- expandSyntaxCommand headTok
        -- Insert those resulting lex-tokens back into the input. (It's not this
        -- function's concern, but recall they will be put on the
        -- lex-token-buffer).
        Lex.insertLexTokensToSource lts
        -- Try to read a primitive token again. Note that the new lex-tokens
        -- might themselves introduce a syntax command, so we might need to
        -- expand again.
        getPrimitiveTokenImpl

runParserDuringExpansion ::
  (MonadError e m, AsType ExpansionError e) =>
  Par.ParseT m a ->
  m a
runParserDuringExpansion parser =
  Par.runParseT parser >>= \case
    Left e -> throwError $ injectTyped $ ExpansionParsingError e
    Right v -> pure v

expandSyntaxCommand ::
  (MonadError e m, AsType ExpansionError e, HSt.MonadHexState m, MonadPrimTokenSource m, Lex.MonadLexTokenSource m) =>
  ST.SyntaxCommandHeadToken ->
  m (Seq Lex.LexToken)
expandSyntaxCommand = \case
  ST.MacroTok macroDefinition -> do
    args <- runParserDuringExpansion $ Par.parseMacroArguments macroDefinition.parameterSpecification
    Expand.substituteArgsIntoMacroBody macroDefinition.replacementText args
  ST.ConditionTok ct -> do
    Expand.expandConditionToken ct
    pure mempty
  -- NumberTok ->
  --   notImplemented "syntax command NumberTok"
  -- RomanNumeralTok ->
  --   notImplemented "syntax command RomanNumeralTok"
  -- StringTok -> do
  --   conf <- use $ typed @Conf.Config
  --   let escapeChar = (Conf.IntParamVal . Conf.lookupIntParameter EscapeChar) conf
  --   expandString escapeChar <$> parseLexToken
  -- JobNameTok ->
  --   notImplemented "syntax command JobNameTok"
  -- FontNameTok ->
  --   notImplemented "syntax command FontNameTok"
  -- MeaningTok ->
  --   notImplemented "syntax command MeaningTok"
  -- CSNameTok -> do
  --   a <- parseCSNameArgs
  --   singleton <$> expandCSName a
  -- ExpandAfterTok -> do
  --   argLT <- takeLexToken
  --   (_, postArgLTs) <- takeAndExpandResolvedToken
  --   -- Prepend the unexpanded token.
  --   pure (argLT <| postArgLTs)
  -- NoExpandTok ->
  --   notImplemented "syntax command NoExpandTok"
  -- MarkRegisterTok _ ->
  --   notImplemented "syntax command MarkRegisterTok"
  -- -- \input ⟨file name⟩:
  -- -- - Expand to no tokens
  -- -- - Prepare to read from the specified file before looking at any more
  -- --   tokens from the current source.
  -- InputTok -> do
  --   TeXFilePath texPath <- parseFileName
  --   inputPath texPath
  --   pure mempty
  -- EndInputTok ->
  --   notImplemented "syntax command EndInputTok"
  -- TheTok -> do
  --   intQuant <- parseInternalQuantity
  --   fmap charCodeAsMadeToken <$> texEvaluate intQuant
  -- ChangeCaseTok direction -> do
  --   conf <- use $ typed @Conf.Config
  --   expandChangeCase
  --     (\c -> Conf.lookupChangeCaseCode direction c conf)
  --     <$> parseGeneralText
  t -> notImplemented $ "Expand syntax command, head: " <> show t
