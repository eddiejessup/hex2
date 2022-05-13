{-# LANGUAGE UndecidableInstances #-}

module Hex.Stage.Expand.Impl where

import Hex.Capability.Log.Interface (MonadHexLog)
import Hex.Common.HexState.Interface qualified as HSt
import Hex.Common.HexState.Interface.Resolve (ResolvedToken (..))
import Hex.Common.HexState.Interface.Resolve.PrimitiveToken (PrimitiveToken)
import Hex.Common.HexState.Interface.Resolve.PrimitiveToken qualified as PT
import Hex.Common.HexState.Interface.Resolve.SyntaxToken qualified as ST
import Hex.Common.Parse.Impl qualified as Par
import Hex.Common.Parse.Interface qualified as Par
import Hex.Stage.Evaluate.Impl.Common qualified as Eval
import Hex.Stage.Evaluate.Impl.SyntaxCommand qualified as Eval
import Hex.Stage.Evaluate.Interface.AST.SyntaxCommand qualified as E
import Hex.Stage.Expand.Impl.Expand qualified as Expand
import Hex.Stage.Expand.Interface (ExpansionError (..), MonadPrimTokenSource (..))
import Hex.Stage.Lex.Interface qualified as Lex
import Hex.Stage.Lex.Interface.Extract (LexToken)
import Hex.Stage.Lex.Interface.Extract qualified as Lex
import Hex.Stage.Parse.Impl.Parsers.SyntaxCommand qualified as Par
import Hex.Stage.Resolve.Interface qualified as Res
import Hexlude
import qualified Hex.Stage.Expand.Interface as Expand

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
  ( Monad m,
    Res.MonadResolve (MonadPrimTokenSourceT m),
    MonadError e (MonadPrimTokenSourceT m),
    AsType ExpansionError e,
    AsType Res.ResolutionError e,
    AsType Eval.EvaluationError e,
    AsType Par.ParsingError e,
    Lex.MonadLexTokenSource (MonadPrimTokenSourceT m),
    HSt.MonadHexState (MonadPrimTokenSourceT m),
    MonadState s m,
    HasType (Expand.ConditionStates) s
  ) =>
  MonadPrimTokenSource (MonadPrimTokenSourceT m)
  where
  getPrimitiveToken = getPrimitiveTokenImpl

  getResolvedToken = getResolvedTokenImpl

  getTokenInhibited = Lex.getLexToken

  pushConditionState condState = modifying' conditionStatesLens (cons condState)

  peekConditionState = use (conditionStatesLens % to headMay)

  popConditionState = use (conditionStatesLens % to uncons) >>= \case
      Nothing -> pure Nothing
      Just (condState, rest) -> do
        assign' conditionStatesLens rest
        pure (Just condState)

conditionStatesLens :: HasType Expand.ConditionStates s => Lens' s [Expand.ConditionState]
conditionStatesLens = typed @Expand.ConditionStates % #unConditionStates

-- Get the next lex-token from the input, resolve it, and expand it if
-- necessary.
-- Note that the lex-token is just returned for debugging really.
-- It is passed through unchanged from the lex-token-source.
getResolvedTokenImpl ::
  ( Res.MonadResolve m,
    MonadError e m,
    AsType Res.ResolutionError e,
    Lex.MonadLexTokenSource m
  ) =>
  m (Maybe (Lex.LexToken, ResolvedToken))
getResolvedTokenImpl =
  Res.getMayResolvedToken >>= \case
    -- If nothing left in the input, return nothing.
    Nothing -> pure Nothing
    -- If we get a token, we only care about the resolved version.
    -- Check if resolution succeeded.
    Just (lt, errOrResolvedTok) -> case errOrResolvedTok of
      -- If resolution failed, throw an error.
      Left e ->
        throwError $ injectTyped e
      Right rt ->
        pure $ Just (lt, rt)

-- Get the next lex-token from the input, resolve it, and expand it if
-- necessary.
-- Note that the lex-token is just returned for debugging really.
-- It is passed through unchanged from the lex-token-source.
-- In order to expand syntax-commands into primitive tokens,
-- we need to be able to parse primitive-token streams.
-- This might seem circular, and it is! But because syntax-commands
-- can need to be expanded recursively, this is needed.
-- This is why we require `MonadPrimTokenSource m` in order to implement this
-- very interface.
getPrimitiveTokenImpl ::
  ( Res.MonadResolve m,
    MonadError e m,
    AsType ExpansionError e,
    AsType Res.ResolutionError e,
    AsType Eval.EvaluationError e,
    AsType Par.ParsingError e,
    Lex.MonadLexTokenSource m,
    MonadPrimTokenSource m,
    HSt.MonadHexState m
  ) =>
  m (Maybe (LexToken, PrimitiveToken))
getPrimitiveTokenImpl =
  getResolvedTokenImpl >>= \case
    Nothing -> pure Nothing
    Just (lt, rt) -> case rt of
      -- If we resolved to a primitive token, we are done, just return that.
      PrimitiveToken pt ->
        pure $ Just (lt, pt)
      -- Otherwise, the token is the head of a syntax-command.
      SyntaxCommandHeadToken headTok -> do
        -- Expand the rest of the command into lex-tokens.
        lts <- parseEvalExpandSyntaxCommand headTok
        -- Insert those resulting lex-tokens back into the input. (It's not this
        -- function's concern, but recall they will be put on the
        -- lex-token-buffer).
        Lex.insertLexTokensToSource lts
        -- Try to read a primitive token again. Note that the new lex-tokens
        -- might themselves introduce a syntax command, so we might need to
        -- expand again.
        getPrimitiveTokenImpl

parseEvalExpandSyntaxCommand ::
  ( MonadError e m,
    AsType ExpansionError e,
    AsType Par.ParsingError e,
    AsType Eval.EvaluationError e,
    HSt.MonadHexState m,
    MonadPrimTokenSource m,
    Lex.MonadLexTokenSource m
  ) =>
  ST.SyntaxCommandHeadToken ->
  m (Seq Lex.LexToken)
parseEvalExpandSyntaxCommand headTok = do
  syntaxCommand <-
    Par.runParseT (Par.headToParseSyntaxCommand headTok) >>= \case
      (Left e, _) -> throwError $ injectTyped e
      (Right v, _) -> pure v
  eSyntaxCommand <- Eval.evalSyntaxCommand syntaxCommand
  expandSyntaxCommand eSyntaxCommand

expandSyntaxCommand ::
  ( MonadError e m,
    AsType ExpansionError e,
    MonadPrimTokenSource m,
    HSt.MonadHexState m
  ) =>
  E.SyntaxCommand ->
  m (Seq Lex.LexToken)
expandSyntaxCommand = \case
  E.CallMacro macroDefinition macroArgumentList -> do
    Expand.substituteArgsIntoMacroBody macroDefinition.replacementText macroArgumentList
  E.ApplyConditionHead conditionOutcome -> do
    Expand.applyConditionOutcome conditionOutcome
    pure mempty
  E.ApplyConditionBody conditionBodyTok -> do
    Expand.applyConditionBody conditionBodyTok
    pure mempty
  E.RenderNumber _n ->
    notImplemented "RenderNumber"
  E.RenderRomanNumeral _n ->
    notImplemented "RenderRomanNumeral"
  E.RenderTokenAsString lt -> do
    escapeCharInt <- HSt.getParameterValue PT.EscapeChar
    pure $ Expand.renderTokenAsString escapeCharInt lt
  -- expandString escapeChar lt
  E.RenderJobName ->
    notImplemented "RenderJobName"
  E.RenderFontName _fontRef ->
    notImplemented "RenderFontName"
  E.RenderTokenMeaning _lt ->
    notImplemented "RenderTokenMeaning"
  E.ParseControlSequence _bytes -> do
    notImplemented "ParseControlSequence"
  -- singleton <$> expandCSName a
  E.ExpandAfter _lt -> do
    notImplemented "ExpandAfter"
  -- (_, postArgLTs) <- takeAndExpandResolvedToken
  -- Prepend the unexpanded token.
  -- pure (argLT <| postArgLTs)
  E.NoExpand _lt ->
    notImplemented "NoExpand"
  E.GetMarkRegister _ ->
    notImplemented "GetMarkRegister"
  -- \input ⟨file name⟩:
  -- - Expand to no tokens
  -- - Prepare to read from the specified file before looking at any more
  --   tokens from the current source.
  E.OpenInputFile _filePath -> do
    void $ notImplemented "OpenInputFile"
    -- inputPath filePath
    pure mempty
  E.EndInputFile ->
    notImplemented "EndInputFile"
  E.RenderInternalQuantity -> do
    notImplemented "RenderInternalQuantity"
  -- fmap charCodeAsMadeToken <$> texEvaluate intQuant
  E.ChangeCase _vDirection -> do
    notImplemented "ChangeCase"

-- expandChangeCase
--   (\c -> Conf.lookupChangeCaseCode direction c conf)
--   <$> parseGeneralText
-- c -> notImplemented $ "Expand syntax command, command: " <> show c
