module Hex.Stage.Expand.Impl.Expand where

import Data.Sequence qualified as Seq
import Hex.Common.HexState.Interface.Resolve qualified as Res
import Hex.Common.HexState.Interface.Resolve.SyntaxToken qualified as ST
import Hex.Stage.Evaluate.Interface.AST.SyntaxCommand qualified as AST
import Hex.Stage.Expand.Interface (ExpansionError)
import Hex.Stage.Expand.Interface qualified as Expand
import Hex.Stage.Lex.Interface.Extract qualified as Lex
import Hex.Stage.Parse.Impl.Parsers.BalancedText qualified as Par
import Hex.Stage.Parse.Interface.AST.SyntaxCommand qualified as Uneval
import Hexlude

substituteArgsIntoMacroBody ::
  forall m e.
  ( MonadError e m,
    AsType ExpansionError e
  ) =>
  ST.MacroReplacementText ->
  Uneval.MacroArgumentList ->
  m (Seq Lex.LexToken)
substituteArgsIntoMacroBody replacementText argsList =
  case replacementText of
    ST.ExpandedMacroReplacementText -> notImplemented "substituteArgsIntoMacroBody: ExpandedReplacementText"
    ST.InhibitedMacroReplacementText inhibitedReplacementText ->
      -- - Map each 'macro-text-token' into a sequence of lex-tokens, using
      --   `renderToken`, and looking up parameter values in `argsList`.
      -- - Flatten the resulting sequence-of-sequences into a sequence of
      -- - lex-tokens.
      -- `foldMapM` does the above in one pass.
      foldMapM renderToken (inhibitedReplacementText.unInhibitedReplacementText)
  where
    -- The contents of a macro replacement text is either:
    -- - An ordinary lex-token. In this case the output is just that lex-token itself.
    -- - A reference to some macro parameter. In this case we should look up the
    --   relevant argument in the argument-list, and that is our output.
    --   If the macro refers to a parameter that isn't present in our argument
    --   list, then the user didn't provide enough arguments.
    renderToken :: ST.MacroTextToken -> m (Seq Lex.LexToken)
    renderToken = \case
      ST.MacroTextLexToken x ->
        pure (Seq.singleton x)
      ST.MacroTextParamToken argIx -> case Uneval.lookupArg argIx argsList of
        Nothing ->
          throwError $ injectTyped $ Expand.MacroArgumentSubstitutionError argIx argsList
        Just arg ->
          pure $ arg.unMacroArgument.unInhibitedBalancedText

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
  forall m e.
  ( MonadError e m,
    AsType ExpansionError e,
    Expand.MonadPrimTokenSource m
  ) =>
  AST.ConditionOutcome ->
  m ()
applyConditionOutcome = \case
  AST.IfConditionOutcome ifConditionOutcome ->
    case ifConditionOutcome of
      AST.SkipPreElseBlock -> do
        skipEndCause <- skipUntilElseOrEndif ElseOrEndif
        case skipEndCause of
          EndedOnElse ->
            -- Push a state to prepare for the later 'end-if'.
            Expand.pushConditionState (Expand.IfConditionState Expand.InUnskippedElseBlock)
          EndedOnEndIf ->
            pure ()
      AST.SkipElseBlock -> do
        Expand.pushConditionState (Expand.IfConditionState Expand.InUnskippedPreElseBlock)
  AST.CaseConditionOutcome _n ->
    notImplemented "Expand case-condition-head"

applyConditionBody ::
  forall m e.
  ( MonadError e m,
    AsType ExpansionError e,
    Expand.MonadPrimTokenSource m
  ) =>
  ST.ConditionBodyTok ->
  m ()
applyConditionBody bodyToken = do
  conditionState <- Expand.peekConditionState
  condState <- case conditionState of
    Nothing -> throwError $ injectTyped $ Expand.UnexpectedConditionBodyToken bodyToken
    Just condState -> pure condState
  case bodyToken of
    ST.EndIf ->
      void $ Expand.popConditionState
    ST.Else ->
      case condState of
        Expand.IfConditionState Expand.InUnskippedPreElseBlock ->
          skipUntilEndOfCondition
        Expand.CaseConditionState Expand.InUnskippedPostOrBlock ->
          skipUntilEndOfCondition
        Expand.IfConditionState Expand.InUnskippedElseBlock ->
          err
        Expand.CaseConditionState Expand.InUnskippedPostElseBlock ->
          err
    ST.Or ->
      case condState of
        Expand.IfConditionState _ ->
          err
        Expand.CaseConditionState Expand.InUnskippedPostOrBlock ->
          skipUntilEndOfCondition
        Expand.CaseConditionState Expand.InUnskippedPostElseBlock ->
          err
  where
    err = throwError $ injectTyped $ Expand.UnexpectedConditionBodyToken bodyToken

    skipUntilEndOfCondition =
      skipUntilElseOrEndif OnlyEndIf >>= \case
        EndedOnElse -> panic "Impossible!"
        EndedOnEndIf -> pure ()

data SkipStopCondition
  = ElseOrEndif
  | OnlyEndIf

data SkipEndCause
  = EndedOnEndIf
  | EndedOnElse

skipUntilElseOrEndif ::
  forall e m.
  (Expand.MonadPrimTokenSource m, MonadError e m, AsType ExpansionError e) =>
  SkipStopCondition ->
  m SkipEndCause
skipUntilElseOrEndif blockTarget = do
  snd <$> Par.parseNestedExpr parseNext
  where
    parseNext depth =
      Expand.getResolvedTokenErrorEOF (injectTyped Expand.EndOfInputWhileSkipping) >>= \case
        -- If we see an 'if', increment the condition depth.
        Res.SyntaxCommandHeadToken (ST.ConditionTok (ST.ConditionHeadTok _)) ->
          pure (EndedOnElse, GT)
        -- If we see an 'end-if', decrement the condition depth.
        Res.SyntaxCommandHeadToken (ST.ConditionTok (ST.ConditionBodyTok ST.EndIf)) ->
          pure (EndedOnEndIf, LT)
        -- If we see an 'else' and are at top condition depth, and our
        -- target block is an else block, we are done skipping tokens.
        Res.SyntaxCommandHeadToken (ST.ConditionTok (ST.ConditionBodyTok ST.Else))
          | depth == 1,
            ElseOrEndif <- blockTarget -> do
              pure (EndedOnElse, LT)
        -- (we ignore 'else's even if it is syntactically wrong, if it's
        -- outside our block of interest.)
        -- Any other token, just skip and continue unchanged.
        _ ->
          pure (EndedOnElse, EQ)
