{-# LANGUAGE UndecidableInstances #-}

module Hex.Stage.Expand.Impl where

import Data.Sequence qualified as Seq
import Hex.Capability.Log.Interface (MonadHexLog)
import Hex.Capability.Log.Interface qualified as Log
import Hex.Common.Codes qualified as Code
import Hex.Common.HexState.Interface qualified as HSt
import Hex.Common.HexState.Interface.Grouped qualified as HSt.Grouped
import Hex.Common.HexState.Interface.Parameter qualified as HSt.Param
import Hex.Common.HexState.Interface.Resolve (ResolvedToken (..))
import Hex.Common.HexState.Interface.Resolve qualified as HSt.Res
import Hex.Common.HexState.Interface.Resolve.PrimitiveToken (PrimitiveToken)
import Hex.Common.HexState.Interface.Resolve.PrimitiveToken qualified as PT
import Hex.Common.HexState.Interface.Resolve.ExpandableToken qualified as ST
import Hex.Common.HexState.Interface.TokenList qualified as HSt.TL
import Hex.Common.Parse.Impl qualified as Par
import Hex.Common.Parse.Interface qualified as Par
import Hex.Common.Quantity qualified as Q
import Hex.Stage.Evaluate.Impl.Common qualified as Eval
import Hex.Stage.Evaluate.Impl.ExpansionCommand qualified as Eval
import Hex.Stage.Evaluate.Interface.AST.ExpansionCommand qualified as E
import Hex.Stage.Expand.Impl.Expand qualified as Expand
import Hex.Stage.Expand.Interface (ExpansionError (..), MonadPrimTokenSource (..))
import Hex.Stage.Expand.Interface qualified as Expand
import Hex.Stage.Lex.Interface qualified as Lex
import Hex.Stage.Lex.Interface.Extract (LexToken)
import Hex.Stage.Lex.Interface.Extract qualified as Lex
import Hex.Stage.Parse.Impl.Parsers.ExpansionCommand qualified as Par
import Hex.Stage.Resolve.Interface qualified as Res
import Hexlude
import qualified Formatting as F

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
    AsType Res.ResolutionError e,
    AsType Eval.EvaluationError e,
    AsType Par.ParsingError e,
    Lex.MonadLexTokenSource (MonadPrimTokenSourceT m),
    HSt.MonadHexState (MonadPrimTokenSourceT m),
    MonadState s m,
    HasType (Expand.ConditionStates) s,
    MonadHexLog m
  ) =>
  MonadPrimTokenSource (MonadPrimTokenSourceT m)
  where
  getPrimitiveToken = do
    Log.log $ "Fetching primitive token (resolving and expanding)"
    getPrimitiveTokenImpl

  getResolvedToken = do
    Log.log $ "Fetching resolved token (resolving but not expanding)"
    getResolvedTokenImpl

  getTokenInhibited = do
    Log.log $ "Fetching lex-token (not resolving or expanding)"
    mayLexToken <- Lex.getLexToken
    case mayLexToken of
      Just lt -> do
        Log.log $ "While inhibited, got lex-token: " <> F.sformat Lex.fmtLexToken lt
      Nothing -> pure ()
    pure mayLexToken

  pushConditionState condState = modifying' conditionStatesLens (cons condState)

  peekConditionState = use (conditionStatesLens % to headMay)

  popConditionState =
    use (conditionStatesLens % to uncons) >>= \case
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
  m (Maybe (LexToken, ResolvedToken))
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
-- In order to expand expansion-commands into primitive tokens,
-- we need to be able to parse primitive-token streams.
-- This might seem circular, and it is! But because expansion-commands
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
    HSt.MonadHexState m,
    MonadHexLog m
  ) =>
  m (Maybe (LexToken, PrimitiveToken))
getPrimitiveTokenImpl =
  getResolvedTokenImpl >>= \case
    Nothing -> pure Nothing
    Just (lt, rt) ->
      expandResolvedTokenImpl rt >>= \case
        UntouchedPrimitiveToken pt ->
          pure $ Just (lt, pt)
        ExpandedToLexTokens lts -> do
          Lex.insertLexTokensToSource lts
          getPrimitiveTokenImpl

data ExpansionResult
  = UntouchedPrimitiveToken PrimitiveToken
  | ExpandedToLexTokens (Seq LexToken)

expandResolvedTokenImpl ::
  ( Res.MonadResolve m,
    MonadError e m,
    AsType ExpansionError e,
    AsType Eval.EvaluationError e,
    AsType Par.ParsingError e,
    AsType Res.ResolutionError e,
    Lex.MonadLexTokenSource m,
    MonadPrimTokenSource m,
    HSt.MonadHexState m,
    MonadHexLog m
  ) =>
  ResolvedToken ->
  m ExpansionResult
expandResolvedTokenImpl = \case
  -- If we resolved to a primitive token, we are done, just return that.
  PrimitiveToken pt ->
    pure $ UntouchedPrimitiveToken pt
  -- Otherwise, the token is the head of an expansion-command.
  ExpansionCommandHeadToken headTok -> do
    -- Expand the rest of the command into lex-tokens.
    ExpandedToLexTokens <$> parseEvalExpandExpansionCommand headTok

expandLexTokenImpl ::
  ( Res.MonadResolve m,
    MonadError e m,
    AsType ExpansionError e,
    AsType Eval.EvaluationError e,
    AsType Par.ParsingError e,
    AsType Res.ResolutionError e,
    Lex.MonadLexTokenSource m,
    MonadPrimTokenSource m,
    HSt.MonadHexState m,
    MonadHexLog m
  ) =>
  LexToken ->
  m ExpansionResult
expandLexTokenImpl lt =
  Res.resolveLexToken lt >>= \case
    Left resolveErr -> throwError $ injectTyped resolveErr
    Right rt -> expandResolvedTokenImpl rt

parseEvalExpandExpansionCommand ::
  ( MonadError e m,
    AsType ExpansionError e,
    AsType Par.ParsingError e,
    AsType Eval.EvaluationError e,
    AsType Res.ResolutionError e,
    HSt.MonadHexState m,
    Res.MonadResolve m,
    MonadPrimTokenSource m,
    Lex.MonadLexTokenSource m,
    MonadHexLog m
  ) =>
  ST.ExpansionCommandHeadToken ->
  m (Seq LexToken)
parseEvalExpandExpansionCommand headTok = do
  expansionCommand <-
    Par.runParseT (Par.headToParseExpansionCommand headTok) >>= \case
      (Left e, _) -> throwError $ injectTyped e
      (Right v, _) -> pure v
  eExpansionCommand <- Eval.evalExpansionCommand expansionCommand
  expandExpansionCommand eExpansionCommand

expandExpansionCommand ::
  ( MonadError e m,
    AsType ExpansionError e,
    AsType Eval.EvaluationError e,
    AsType Par.ParsingError e,
    AsType Res.ResolutionError e,
    Res.MonadResolve m,
    Lex.MonadLexTokenSource m,
    MonadPrimTokenSource m,
    HSt.MonadHexState m,
    MonadHexLog m
  ) =>
  E.ExpansionCommand ->
  m (Seq LexToken)
expandExpansionCommand = \case
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
  E.RenderTokenAsTokens lt -> do
    escapeCharInt <- HSt.getParameterValue (HSt.Param.IntQuantParam HSt.Param.EscapeChar)
    pure $ Expand.renderTokenAsTokens escapeCharInt lt
  -- expandString escapeChar lt
  E.RenderJobName ->
    notImplemented "RenderJobName"
  E.RenderFontName _fontRef ->
    notImplemented "RenderFontName"
  E.RenderTokenMeaning _lt ->
    notImplemented "RenderTokenMeaning"
  E.ParseControlSequence cs -> do
    let controlSymbol = HSt.Res.ControlSequenceSymbol cs
    HSt.resolveSymbol controlSymbol >>= \case
      Just _ -> pure ()
      Nothing -> HSt.setSymbol controlSymbol (PrimitiveToken PT.RelaxTok) HSt.Grouped.Local
    pure $ Seq.singleton $ Lex.ControlSequenceLexToken cs
  -- singleton <$> expandCSName a
  E.ExpandAfter noExpandLexToken toExpandLexToken -> do
    expandedLexTokens <-
      expandLexTokenImpl toExpandLexToken <&> \case
        UntouchedPrimitiveToken _ -> pure toExpandLexToken
        ExpandedToLexTokens lts -> lts
    -- Prepend the unexpanded token.
    pure (noExpandLexToken <| expandedLexTokens)
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
  E.RenderInternalQuantity internalQuantity -> do
    pure $ Expand.renderInternalQuantity internalQuantity
  E.ChangeCase vDirection inhibText ->
    forM (inhibText.unInhibitedBalancedText.unBalancedText) $ \lt ->
      case lt of
        Lex.ControlSequenceLexToken _ ->
          pure lt
        Lex.CharCatLexToken lexCharCat -> do
          changeCaseCode <- case vDirection of
            Q.Upward -> do
              ucCode <- HSt.getHexCode (Code.CUpperCaseCodeType) lexCharCat.lexCCChar
              pure $ ucCode ^. typed @Code.ChangeCaseCode
            Q.Downward -> do
              lcCode <- HSt.getHexCode (Code.CLowerCaseCodeType) lexCharCat.lexCCChar
              pure $ lcCode ^. typed @Code.ChangeCaseCode
          pure $ case changeCaseCode of
            Code.NoCaseChange ->
              lt
            Code.ChangeToCode uc ->
              lt & _Typed @Lex.LexCharCat % typed @Code.CharCode .~ uc
