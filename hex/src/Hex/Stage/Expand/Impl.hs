{-# LANGUAGE UndecidableInstances #-}

module Hex.Stage.Expand.Impl where

import Data.Sequence qualified as Seq
import Hex.Capability.Log.Interface (MonadHexLog)
import Hex.Common.Codes qualified as Code
import Hex.Common.HexInput.Interface qualified as HIn
import Hex.Common.HexState.Interface qualified as HSt
import Hex.Common.HexState.Interface.Grouped qualified as HSt.Grouped
import Hex.Common.HexState.Interface.Parameter qualified as HSt.Param
import Hex.Common.HexState.Interface.Resolve qualified as HSt.Res
import Hex.Common.HexState.Interface.TokenList qualified as HSt.TL
import Hex.Common.Parse.Impl qualified as Par
import Hex.Common.Parse.Interface qualified as Par
import Hex.Common.Quantity qualified as Q
import Hex.Common.Token.Lexed qualified as LT
import Hex.Common.Token.Resolved qualified as RT
import Hex.Common.Token.Resolved.Expandable qualified as ST
import Hex.Common.Token.Resolved.Primitive (PrimitiveToken)
import Hex.Common.Token.Resolved.Primitive qualified as PT
import Hex.Stage.Evaluate.Impl.Common qualified as Eval
import Hex.Stage.Evaluate.Impl.ExpansionCommand qualified as Eval
import Hex.Stage.Evaluate.Interface.AST.ExpansionCommand qualified as E
import Hex.Stage.Expand.Impl.Expand qualified as Expand
import Hex.Stage.Expand.Interface (ExpansionError (..), MonadPrimTokenSource (..))
import Hex.Stage.Expand.Interface qualified as Expand
import Hex.Stage.Parse.Impl.Parsers.ExpansionCommand qualified as Par
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
      HIn.MonadHexInput,
      Par.MonadPrimTokenParse,
      HSt.MonadHexState,
      MonadHexLog
    )

instance
  ( MonadError e (MonadPrimTokenSourceT m),
    AsType ExpansionError e,
    AsType Eval.EvaluationError e,
    AsType Par.ParsingError e,
    AsType HSt.ResolutionError e,
    HIn.MonadHexInput (MonadPrimTokenSourceT m),
    HSt.MonadHexState (MonadPrimTokenSourceT m),
    MonadState s m,
    HasType (Expand.ConditionStates) s,
    MonadHexLog m
  ) =>
  MonadPrimTokenSource (MonadPrimTokenSourceT m)
  where
  getPrimitiveToken = getPrimitiveTokenImpl

  getResolvedToken = getResolvedTokenImpl

  getTokenInhibited = HIn.getNextLexToken

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
  ( MonadError e m,
    AsType HSt.ResolutionError e,
    HIn.MonadHexInput m,
    HSt.MonadHexState m
  ) =>
  m (Maybe (LT.LexToken, RT.ResolvedToken))
getResolvedTokenImpl =
  HIn.getMayResolvedToken >>= \case
    -- If nothing left in the input, return nothing.
    Nothing -> pure Nothing
    -- If we get a token, we only care about the resolved version.
    -- Check if resolution succeeded.
    Just (lt, errOrRT) -> case errOrRT of
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
  ( MonadError e m,
    AsType ExpansionError e,
    AsType Eval.EvaluationError e,
    AsType Par.ParsingError e,
    AsType HSt.ResolutionError e,
    HIn.MonadHexInput m,
    MonadPrimTokenSource m,
    HSt.MonadHexState m,
    MonadHexLog m
  ) =>
  m (Maybe (LT.LexToken, PrimitiveToken))
getPrimitiveTokenImpl =
  getResolvedTokenImpl >>= \case
    Nothing -> pure Nothing
    Just (lt, rt) ->
      expandResolvedTokenImpl rt >>= \case
        UntouchedPrimitiveToken pt ->
          pure $ Just (lt, pt)
        ExpandedToLexTokens lts -> do
          HIn.insertLexTokens lts
          getPrimitiveTokenImpl

data ExpansionResult
  = UntouchedPrimitiveToken PrimitiveToken
  | ExpandedToLexTokens (Seq LT.LexToken)

expandResolvedTokenImpl ::
  ( MonadError e m,
    AsType ExpansionError e,
    AsType Eval.EvaluationError e,
    AsType Par.ParsingError e,
    AsType HSt.ResolutionError e,
    HIn.MonadHexInput m,
    MonadPrimTokenSource m,
    HSt.MonadHexState m,
    MonadHexLog m
  ) =>
  RT.ResolvedToken ->
  m ExpansionResult
expandResolvedTokenImpl = \case
  -- If we resolved to a primitive token, we are done, just return that.
  RT.PrimitiveToken pt ->
    pure $ UntouchedPrimitiveToken pt
  -- Otherwise, the token is the head of an expansion-command.
  RT.ExpansionCommandHeadToken headTok -> do
    -- Expand the rest of the command into lex-tokens.
    ExpandedToLexTokens <$> parseEvalExpandExpansionCommand headTok

expandLexTokenImpl ::
  ( MonadError e m,
    AsType ExpansionError e,
    AsType Eval.EvaluationError e,
    AsType Par.ParsingError e,
    AsType HSt.ResolutionError e,
    HIn.MonadHexInput m,
    MonadPrimTokenSource m,
    HSt.MonadHexState m,
    MonadHexLog m
  ) =>
  LT.LexToken ->
  m ExpansionResult
expandLexTokenImpl lt =
  HSt.resolveLexToken lt >>= \case
    Left resolveErr -> throwError $ injectTyped resolveErr
    Right rt -> expandResolvedTokenImpl rt

parseEvalExpandExpansionCommand ::
  ( MonadError e m,
    AsType ExpansionError e,
    AsType Par.ParsingError e,
    AsType Eval.EvaluationError e,
    AsType HSt.ResolutionError e,
    HSt.MonadHexState m,
    MonadPrimTokenSource m,
    HIn.MonadHexInput m,
    MonadHexLog m
  ) =>
  ST.ExpansionCommandHeadToken ->
  m (Seq LT.LexToken)
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
    AsType HSt.ResolutionError e,
    HIn.MonadHexInput m,
    MonadPrimTokenSource m,
    HSt.MonadHexState m,
    MonadHexLog m
  ) =>
  E.ExpansionCommand ->
  m (Seq LT.LexToken)
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
      Nothing -> HSt.setSymbol controlSymbol (RT.PrimitiveToken PT.RelaxTok) HSt.Grouped.Local
    pure $ Seq.singleton $ LT.ControlSequenceLexToken cs
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
  E.OpenInputFile filePath -> do
    HIn.openInputFile filePath
    pure mempty
  E.EndInputFile ->
    notImplemented "EndInputFile"
  E.RenderInternalQuantity internalQuantity -> do
    pure $ Expand.renderInternalQuantity internalQuantity
  E.ChangeCase vDirection inhibText ->
    forM (inhibText.unInhibitedBalancedText.unBalancedText) $ \lt ->
      case lt of
        LT.ControlSequenceLexToken _ ->
          pure lt
        LT.CharCatLexToken lexCharCat -> do
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
              lt & _Typed @LT.LexCharCat % typed @Code.CharCode .~ uc
