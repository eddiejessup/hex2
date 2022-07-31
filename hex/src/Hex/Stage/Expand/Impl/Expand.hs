module Hex.Stage.Expand.Impl.Expand where

import Data.Sequence qualified as Seq
import Formatting qualified as F
import Hex.Common.Codes qualified as Code
import Hex.Common.HexState.Interface qualified as HSt
import Hex.Common.HexState.Interface.TokenList qualified as HSt.TL
import Hex.Common.Quantity qualified as Q
import Hex.Common.Token.Lexed qualified as LT
import Hex.Common.Token.Resolved qualified as RT
import Hex.Common.Token.Resolved.Expandable qualified as ST
import Hex.Stage.Evaluate.Interface.AST.ExpansionCommand qualified as AST
import Hex.Stage.Evaluate.Interface.AST.Quantity qualified as Eval
import Hex.Stage.Expand.Interface (ExpansionError)
import Hex.Stage.Expand.Interface qualified as Expand
import Hex.Stage.Parse.Impl.Parsers.BalancedText qualified as Par
import Hex.Stage.Parse.Interface.AST.ExpansionCommand qualified as Uneval
import Hex.Stage.Read.Interface qualified as HIn
import Hexlude

substituteArgsIntoMacroBody ::
  forall es.
  Error ExpansionError :> es =>
  ST.MacroReplacementText ->
  Uneval.MacroArgumentList ->
  Eff es (Seq LT.LexToken)
substituteArgsIntoMacroBody replacementText argsList =
  -- - Map each 'macro-text-token' into a sequence of lex-tokens, using
  --   `renderToken`, and looking up parameter values in `argsList`.
  -- - Flatten the resulting sequence-of-sequences into a sequence of
  -- - lex-tokens.
  -- `foldMapM` does the above in one pass.
  foldMapM renderToken replacementText.unMacroReplacementText
  where
    -- The contents of a macro replacement text is either:
    -- - An ordinary lex-token. In this case the output is just that lex-token itself.
    -- - A reference to some macro parameter. In this case we should look up the
    --   relevant argument in the argument-list, and that is our output.
    --   If the macro refers to a parameter that isn't present in our argument
    --   list, then the user didn't provide enough arguments.
    renderToken :: ST.MacroTextToken -> Eff es (Seq LT.LexToken)
    renderToken = \case
      ST.MacroTextLexToken x ->
        pure (Seq.singleton x)
      ST.MacroTextParamToken argIx -> case Uneval.lookupArg argIx argsList of
        Nothing ->
          throwError $ Expand.MacroArgumentSubstitutionError argIx argsList
        Just arg ->
          pure $ arg.unMacroArgument.unInhibitedBalancedText.unBalancedText

-- If the condition is an 'if':
-- If true:
--   We shouldn't skip any tokens yet, but we should push an if-state telling us
--   to skip tokens once we see an 'else', and pop the state once we see an
--   'endif'.
-- If false:
--   We should skip tokens until we see an 'else' or 'endif'. If we see any
--   condition-tokens along the way, we won't do anything about it, but we need
--   to keep track of our depth to know when our matching 'else' or 'endif'
--   appears.
--   If we end by seeing an 'else':
--     We should stop skipping tokens, but push a state that we will pop when we
--     see the 'endif'.
--   If we end by seeing an 'endif':
--     We are done skipping tokens, done with this 'if'-condition, nothing more
--     to be done.
applyConditionOutcome ::
  forall es.
  ( Error ExpansionError :> es,
    Error HSt.ResolutionError :> es,
    HIn.HexInput :> es,
    HSt.EHexState :> es,
    State Expand.ConditionStates :> es
    -- Expand.PrimTokenSource :> es
  ) =>
  AST.ConditionOutcome ->
  Eff es ()
applyConditionOutcome = \case
  AST.IfConditionOutcome AST.SkipPreElseBlock ->
    skipUntilElseOrEndif ElseOrEndif getPresentResolvedToken >>= \case
      Nothing ->
        pure ()
      Just newState ->
        -- Push a state to prepare for the later 'end-if'.
        pushConditionStateImpl (Expand.IfConditionState newState)
  AST.IfConditionOutcome AST.SkipElseBlock ->
    pushConditionStateImpl (Expand.IfConditionState Expand.InSelectedPreElseIfBlock)
  AST.CaseConditionOutcome tgtBlock ->
    skipUpToCaseBlock tgtBlock getPresentResolvedToken >>= \case
      Nothing -> pure ()
      Just newState ->
        pushConditionStateImpl (Expand.CaseConditionState newState)

pushConditionStateImpl ::
  State Expand.ConditionStates :> es =>
  Expand.ConditionState ->
  Eff es ()
pushConditionStateImpl condState = modifying @Expand.ConditionStates conditionStatesLens (cons condState)

peekConditionStateImpl ::
  State Expand.ConditionStates :> es =>
  Eff es (Maybe Expand.ConditionState)
peekConditionStateImpl = use @Expand.ConditionStates (conditionStatesLens % to headMay)

popConditionStateImpl ::
  State Expand.ConditionStates :> es =>
  Eff es (Maybe Expand.ConditionState)
popConditionStateImpl =
  use @Expand.ConditionStates (conditionStatesLens % to uncons) >>= \case
    Nothing -> pure Nothing
    Just (condState, rest) -> do
      assign conditionStatesLens rest
      pure (Just condState)

conditionStatesLens :: Lens' Expand.ConditionStates [Expand.ConditionState]
conditionStatesLens = #unConditionStates

skipUpToCaseBlock ::
  forall m.
  Monad m =>
  Q.HexInt ->
  m RT.ResolvedToken ->
  m (Maybe Expand.CaseState)
skipUpToCaseBlock tgtBlock getNextToken = go Q.zeroInt 1
  where
    go :: Q.HexInt -> Int -> m (Maybe Expand.CaseState)
    go currentCaseBlock depth
      | depth == 1,
        currentCaseBlock == tgtBlock =
          -- If we are at top condition depth,
          pure $ Just Expand.InSelectedOrCaseBlock
      | otherwise =
          getNextToken >>= \case
            RT.ExpansionCommandHeadToken (ST.ConditionTok (ST.ConditionHeadTok _)) ->
              go currentCaseBlock $ succ depth
            RT.ExpansionCommandHeadToken (ST.ConditionTok (ST.ConditionBodyTok ST.EndIf))
              | depth == 1 ->
                  pure Nothing
              | otherwise ->
                  go currentCaseBlock $ pred depth
            RT.ExpansionCommandHeadToken (ST.ConditionTok (ST.ConditionBodyTok ST.Else))
              | depth == 1 ->
                  pure $ Just Expand.InSelectedElseCaseBlock
            RT.ExpansionCommandHeadToken (ST.ConditionTok (ST.ConditionBodyTok ST.Or))
              | depth == 1 ->
                  go (succ currentCaseBlock) depth
            _ ->
              go currentCaseBlock depth

applyConditionBody ::
  forall es.
  [Error ExpansionError, Error HSt.ResolutionError, HIn.HexInput, HSt.EHexState, State Expand.ConditionStates] :>> es =>
  ST.ConditionBodyTok ->
  Eff es ()
applyConditionBody bodyToken = do
  conditionState <- peekConditionStateImpl
  condState <- case conditionState of
    Nothing -> throwError $ Expand.UnexpectedConditionBodyToken bodyToken
    Just condState -> pure condState
  case bodyToken of
    ST.EndIf ->
      void $ popConditionStateImpl
    ST.Else ->
      case condState of
        Expand.IfConditionState Expand.InSelectedPreElseIfBlock ->
          skipUntilEndOfCondition
        Expand.CaseConditionState Expand.InSelectedOrCaseBlock ->
          skipUntilEndOfCondition
        Expand.IfConditionState Expand.InSelectedElseIfBlock ->
          err
        Expand.CaseConditionState Expand.InSelectedElseCaseBlock ->
          err
    ST.Or ->
      case condState of
        Expand.IfConditionState _ ->
          err
        Expand.CaseConditionState Expand.InSelectedOrCaseBlock ->
          skipUntilEndOfCondition
        Expand.CaseConditionState Expand.InSelectedElseCaseBlock ->
          err
  where
    err = throwError $ Expand.UnexpectedConditionBodyToken bodyToken

    skipUntilEndOfCondition =
      skipUntilElseOrEndif OnlyEndIf getPresentResolvedToken >>= \case
        Just _ -> panic "Impossible!"
        Nothing -> pure ()

data SkipStopCondition
  = ElseOrEndif
  | OnlyEndIf

getPresentResolvedToken ::
  forall es.
  [Error ExpansionError, Error HSt.ResolutionError, HIn.HexInput, HSt.EHexState] :>> es =>
  Eff es RT.ResolvedToken
getPresentResolvedToken = do
  snd <$> nothingToError Expand.EndOfInputWhileSkipping HIn.getResolvedToken

skipUntilElseOrEndif ::
  forall m.
  Monad m =>
  SkipStopCondition ->
  m RT.ResolvedToken ->
  m (Maybe Expand.IfState)
skipUntilElseOrEndif blockTarget getNextToken = do
  snd <$> Par.parseNestedExpr parseNext
  where
    parseNext depth =
      getNextToken <&> \case
        -- If we see an 'if', increment the condition depth.
        RT.ExpansionCommandHeadToken (ST.ConditionTok (ST.ConditionHeadTok _)) ->
          (Nothing, GT)
        -- If we see an 'end-if', decrement the condition depth.
        RT.ExpansionCommandHeadToken (ST.ConditionTok (ST.ConditionBodyTok ST.EndIf)) ->
          (Nothing, LT)
        -- If we see an 'else' and are at top condition depth, and our
        -- target block is an else block, we are done skipping tokens.
        RT.ExpansionCommandHeadToken (ST.ConditionTok (ST.ConditionBodyTok ST.Else))
          | depth == 1,
            ElseOrEndif <- blockTarget ->
              (Just Expand.InSelectedElseIfBlock, LT)
        -- (we ignore 'else's even if it is syntactically wrong, if it's
        -- outside our block of interest.)
        -- Any other token, just skip and continue unchanged.
        _ ->
          (Nothing, EQ)

renderTokenAsTokens ::
  Q.HexInt ->
  LT.LexToken ->
  Seq LT.LexToken
renderTokenAsTokens escapeCharCodeInt tok =
  charCodeAsMadeToken <$> LT.renderTokenAsCodes escapeCharCodeInt tok

-- For \number, \romannumeral, \string. \meaning, \jobname, and \fontname: Each
-- character code gets category "other" , except that 32 gets "space".
charCodeAsMadeToken :: Code.CharCode -> LT.LexToken
charCodeAsMadeToken c =
  LT.CharCatLexToken $
    LT.LexCharCat c $ case c of
      Code.Chr_ ' ' -> Code.Space
      _ -> Code.Other

renderInternalQuantity :: Eval.InternalQuantity -> Seq LT.LexToken
renderInternalQuantity = \case
  Eval.InternalIntQuantity n ->
    tokensFromInt n
  Eval.InternalLengthQuantity length ->
    tokensFromInt length.unLength
  Eval.InternalGlueQuantity glue ->
    tokensFromText (F.sformat Q.fmtGlue glue)
  Eval.InternalMathGlueQuantity mathGlue ->
    tokensFromText (F.sformat Q.fmtMathGlue mathGlue)
  Eval.FontQuantity _fontRef ->
    notImplemented "render FontQuantity"
  Eval.TokenListVariableQuantity _tokenList ->
    notImplemented "render TokenListVariableQuantity"
  where
    -- TODO: This is quite a sloppy implementation, using Int's `Show` instance.
    tokensFromInt :: Q.HexInt -> Seq LT.LexToken
    tokensFromInt n = tokensFromText (show n.unHexInt)

    tokensFromText :: Text -> Seq LT.LexToken
    tokensFromText t = Seq.fromList $ charCodeAsMadeToken <$> (Code.textAsCharCodes t)
