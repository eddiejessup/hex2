{-# LANGUAGE UndecidableInstances #-}

module Hex.Stage.Expand.Impl where

import Data.Sequence qualified as Seq
import Effectful.Dispatch.Dynamic (localSeqUnlift)
import Formatting qualified as F
import Hex.Capability.Log.Interface (HexLog)
import Hex.Common.Codes qualified as Code
import Hex.Common.HexInput.Interface qualified as HIn
import Hex.Common.HexState.Interface qualified as HSt
import Hex.Common.HexState.Interface.Grouped qualified as HSt.Grouped
import Hex.Common.HexState.Interface.Parameter qualified as HSt.Param
import Hex.Common.HexState.Interface.Resolve qualified as HSt.Res
import Hex.Common.HexState.Interface.TokenList qualified as HSt.TL
import Hex.Common.Token.Lexed qualified as LT
import Hex.Common.Token.Resolved qualified as RT
import Hex.Common.Token.Resolved.Expandable qualified as ST
import Hex.Common.Token.Resolved.Primitive (PrimitiveToken)
import Hex.Common.Token.Resolved.Primitive qualified as PT
import Hex.Stage.Evaluate.Impl.Common qualified as Eval
import Hex.Stage.Evaluate.Impl.ExpansionCommand qualified as Eval
import Hex.Stage.Evaluate.Interface.AST.ExpansionCommand qualified as E
import Hex.Stage.Expand.Impl.Expand qualified as Expand
import Hex.Stage.Expand.Interface (ExpansionError (..), ParseUnexpectedError (..), ParsingError (..), PrimTokenSource (..))
import Hex.Stage.Expand.Interface qualified as Expand
import Hex.Stage.Parse.Impl.Parsers.ExpansionCommand qualified as Par
import Hexlude

runPrimTokenSource ::
  [ Error ExpansionError,
    Error ParsingError,
    Error Eval.EvaluationError,
    Error HSt.ResolutionError,
    HIn.HexInput,
    HSt.EHexState,
    State Expand.ConditionStates,
    HexLog,
    EAlternative
  ]
    :>> es =>
  Eff (PrimTokenSource : es) a ->
  Eff es a
runPrimTokenSource = interpret $ \env -> \case
  GetPrimitiveToken -> getPrimitiveTokenImpl
  PushConditionState condState -> Expand.pushConditionStateImpl condState
  PopConditionState -> Expand.popConditionStateImpl
  PeekConditionState -> Expand.peekConditionStateImpl
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

runAltPrimTokenSource ::
  [ Error ExpansionError,
    Error ParsingError,
    Error Eval.EvaluationError,
    Error HSt.ResolutionError,
    HIn.HexInput,
    HSt.EHexState,
    State Expand.ConditionStates,
    HexLog
  ]
    :>> es =>
  Eff (PrimTokenSource : EAlternative : es) a ->
  Eff es a
runAltPrimTokenSource = runAlt . runPrimTokenSource

runAlt :: Error ParsingError :> es => Eff (EAlternative : es) a -> Eff es a
runAlt = interpret $ \env -> \case
  AltEmpty -> throwError @ParsingError $ Expand.UnexpectedParsingError ParseDefaultFailure
  AltChoice a b -> do
    localSeqUnlift env $ \unlift -> do
      catchError @ParsingError (unlift a) $ \_callStack _e -> unlift b

runAltPrimTokenSourceMaybe ::
  [ Error ExpansionError,
    Error ParsingError,
    Error Eval.EvaluationError,
    Error HSt.ResolutionError,
    HIn.HexInput,
    HSt.EHexState,
    State Expand.ConditionStates,
    HexLog
  ]
    :>> es =>
  Eff (PrimTokenSource : EAlternative : Error ParsingError : es) a ->
  Eff es (Maybe a)
runAltPrimTokenSourceMaybe ef = do
  runErrorNoCallStack @ParsingError (runAltPrimTokenSource ef) >>= \case
    Left EndOfInputParsingError -> pure Nothing
    Left e -> throwError e
    Right v -> pure $ Just v

data ExpansionResult
  = UntouchedPrimitiveToken PrimitiveToken
  | ExpandedToLexTokens (Seq LT.LexToken)

expandResolvedTokenImpl ::
  [ Error ExpansionError,
    Error Eval.EvaluationError,
    Error ParsingError,
    Error HSt.ResolutionError,
    State Expand.ConditionStates,
    HIn.HexInput,
    EAlternative,
    HSt.EHexState,
    HexLog
  ]
    :>> es =>
  RT.ResolvedToken ->
  Eff es ExpansionResult
expandResolvedTokenImpl = \case
  -- If we resolved to a primitive token, we are done, just return that.
  RT.PrimitiveToken pt ->
    pure $ UntouchedPrimitiveToken pt
  -- Otherwise, the token is the head of an expansion-command.
  RT.ExpansionCommandHeadToken headTok -> do
    -- Expand the rest of the command into lex-tokens.
    expansionCommand <- runPrimTokenSource $ Par.headToParseExpansionCommand headTok
    eExpansionCommand <- Eval.evalExpansionCommand expansionCommand
    ExpandedToLexTokens <$> expandExpansionCommand eExpansionCommand

expandLexTokenImpl ::
  [ Error ExpansionError,
    Error Eval.EvaluationError,
    Error ParsingError,
    Error HSt.ResolutionError,
    State Expand.ConditionStates,
    EAlternative,
    HIn.HexInput,
    HSt.EHexState,
    HexLog
  ]
    :>> es =>
  LT.LexToken ->
  Eff es ExpansionResult
expandLexTokenImpl lt = do
  HSt.resolveLexToken lt >>= expandResolvedTokenImpl

-- Get the next lex-token from the input, resolve it, and expand it if
-- necessary.
getPrimitiveTokenImpl ::
  [ Error ExpansionError,
    Error Eval.EvaluationError,
    Error ParsingError,
    Error HSt.ResolutionError,
    State Expand.ConditionStates,
    EAlternative,
    HIn.HexInput,
    HSt.EHexState,
    HexLog
  ]
    :>> es =>
  Eff es (Maybe (LT.LexToken, PrimitiveToken))
getPrimitiveTokenImpl =
  HIn.getResolvedToken >>= \case
    Nothing -> pure Nothing
    Just (lt, rt) ->
      expandResolvedTokenImpl rt >>= \case
        UntouchedPrimitiveToken pt ->
          pure $ Just (lt, pt)
        ExpandedToLexTokens lts -> do
          HIn.insertLexTokens lts
          getPrimitiveTokenImpl

expandExpansionCommand ::
  [ Error ExpansionError,
    Error Eval.EvaluationError,
    Error ParsingError,
    Error HSt.ResolutionError,
    State Expand.ConditionStates,
    EAlternative,
    HIn.HexInput,
    HSt.EHexState,
    HexLog
  ]
    :>> es =>
  E.ExpansionCommand ->
  Eff es (Seq LT.LexToken)
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
      Nothing -> HSt.setSymbol controlSymbol (RT.PrimitiveToken PT.RelaxTok) HSt.Grouped.LocalScope
    pure $ Seq.singleton $ LT.ControlSequenceLexToken cs
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
            Upward -> do
              ucCode <- HSt.getHexCode (Code.CUpperCaseCodeType) lexCharCat.lexCCChar
              pure $ ucCode ^. typed @Code.ChangeCaseCode
            Downward -> do
              lcCode <- HSt.getHexCode (Code.CLowerCaseCodeType) lexCharCat.lexCCChar
              pure $ lcCode ^. typed @Code.ChangeCaseCode
          pure $ case changeCaseCode of
            Code.NoCaseChange ->
              lt
            Code.ChangeToCode uc ->
              lt & _Typed @LT.LexCharCat % typed @Code.CharCode !~ uc

satisfyThenExpandingImpl ::
  [ Error ExpansionError,
    Error Eval.EvaluationError,
    Error ParsingError,
    Error HSt.ResolutionError,
    State Expand.ConditionStates,
    EAlternative,
    HIn.HexInput,
    HSt.EHexState,
    HexLog
  ]
    :>> es =>
  ((LT.LexToken, PT.PrimitiveToken) -> Maybe a) ->
  Eff es a
satisfyThenExpandingImpl f =
  satisfyThenCommon
    getPrimitiveTokenImpl
    f

satisfyThenInhibitedImpl ::
  (HIn.HexInput :> es, Error ParsingError :> es) =>
  (LT.LexToken -> Maybe a) ->
  Eff es a
satisfyThenInhibitedImpl f =
  -- Wrap and unwrap using the trivial tuple to make a common interface we can share with the 'expanding' version.
  satisfyThenCommon
    (HIn.getNextLexToken <&> (fmap (,())))
    (\(lt, ()) -> f lt)

satisfyThenCommon ::
  (HIn.HexInput :> es, Error ParsingError :> es) =>
  Eff es (Maybe (LT.LexToken, b)) ->
  ((LT.LexToken, b) -> Maybe a) ->
  Eff es a
satisfyThenCommon parser f = do
  parser >>= \case
    Nothing ->
      throwError @ParsingError $ Expand.UnexpectedParsingError $ ParseExplicitFailure "satisfyThen, no next primitive token"
    Just x@(lt, _) -> do
      case f x of
        Nothing -> do
          HIn.insertLexToken lt
          throwError @ParsingError $ Expand.UnexpectedParsingError $ ParseExplicitFailure $ "satisfyThen, test failed on lex-token: " <> F.sformat LT.fmtLexToken lt
        Just a -> do
          pure a
