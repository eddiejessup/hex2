module Hex.Stage.Expand.Impl.Expand where

import Data.Sequence qualified as Seq
import Formatting qualified as F
import Hex.Common.Codes qualified as Code
import Hex.Common.HexState.Interface.Resolve qualified as Res
import Hex.Common.HexState.Interface.Resolve.ExpandableToken qualified as ST
import Hex.Common.HexState.Interface.TokenList qualified as HSt.TL
import Hex.Common.Quantity qualified as Q
import Hex.Stage.Evaluate.Interface.AST.Quantity qualified as Eval
import Hex.Stage.Evaluate.Interface.AST.ExpansionCommand qualified as AST
import Hex.Stage.Expand.Interface (ExpansionError)
import Hex.Stage.Expand.Interface qualified as Expand
import Hex.Stage.Lex.Interface.Extract qualified as Lex
import Hex.Stage.Parse.Impl.Parsers.BalancedText qualified as Par
import Hex.Stage.Parse.Interface.AST.ExpansionCommand qualified as Uneval
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
  forall m e.
  ( MonadError e m,
    AsType ExpansionError e,
    Expand.MonadPrimTokenSource m
  ) =>
  AST.ConditionOutcome ->
  m ()
applyConditionOutcome = \case
  AST.IfConditionOutcome AST.SkipPreElseBlock ->
    skipUntilElseOrEndif ElseOrEndif getPresentResolvedToken >>= \case
      Nothing ->
        pure ()
      Just newState ->
        -- Push a state to prepare for the later 'end-if'.
        Expand.pushConditionState (Expand.IfConditionState newState)
  AST.IfConditionOutcome AST.SkipElseBlock ->
    Expand.pushConditionState (Expand.IfConditionState Expand.InSelectedPreElseIfBlock)
  AST.CaseConditionOutcome tgtBlock ->
    skipUpToCaseBlock tgtBlock getPresentResolvedToken >>= \case
      Nothing -> pure ()
      Just newState ->
        Expand.pushConditionState (Expand.CaseConditionState newState)

skipUpToCaseBlock ::
  forall m.
  Monad m =>
  Q.HexInt ->
  (m Res.ResolvedToken) ->
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
            Res.ExpansionCommandHeadToken (ST.ConditionTok (ST.ConditionHeadTok _)) ->
              go currentCaseBlock $ succ depth
            Res.ExpansionCommandHeadToken (ST.ConditionTok (ST.ConditionBodyTok ST.EndIf))
              | depth == 1 ->
                  pure Nothing
              | otherwise ->
                  go currentCaseBlock $ pred depth
            Res.ExpansionCommandHeadToken (ST.ConditionTok (ST.ConditionBodyTok ST.Else))
              | depth == 1 ->
                  pure $ Just Expand.InSelectedElseCaseBlock
            Res.ExpansionCommandHeadToken (ST.ConditionTok (ST.ConditionBodyTok ST.Or))
              | depth == 1 ->
                  go (succ currentCaseBlock) depth
            _ ->
              go currentCaseBlock depth

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
    err = throwError $ injectTyped $ Expand.UnexpectedConditionBodyToken bodyToken

    skipUntilEndOfCondition =
      skipUntilElseOrEndif OnlyEndIf getPresentResolvedToken >>= \case
        Just _ -> panic "Impossible!"
        Nothing -> pure ()

data SkipStopCondition
  = ElseOrEndif
  | OnlyEndIf

getPresentResolvedToken ::
  forall m e.
  (Expand.MonadPrimTokenSource m, MonadError e m, AsType ExpansionError e) =>
  m Res.ResolvedToken
getPresentResolvedToken = Expand.getResolvedTokenErrorEOF (injectTyped Expand.EndOfInputWhileSkipping)

skipUntilElseOrEndif ::
  forall m.
  Monad m =>
  SkipStopCondition ->
  m Res.ResolvedToken ->
  m (Maybe Expand.IfState)
skipUntilElseOrEndif blockTarget getNextToken = do
  snd <$> Par.parseNestedExpr parseNext
  where
    parseNext depth =
      getNextToken <&> \case
        -- If we see an 'if', increment the condition depth.
        Res.ExpansionCommandHeadToken (ST.ConditionTok (ST.ConditionHeadTok _)) ->
          (Nothing, GT)
        -- If we see an 'end-if', decrement the condition depth.
        Res.ExpansionCommandHeadToken (ST.ConditionTok (ST.ConditionBodyTok ST.EndIf)) ->
          (Nothing, LT)
        -- If we see an 'else' and are at top condition depth, and our
        -- target block is an else block, we are done skipping tokens.
        Res.ExpansionCommandHeadToken (ST.ConditionTok (ST.ConditionBodyTok ST.Else))
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
  Lex.LexToken ->
  Seq Lex.LexToken
renderTokenAsTokens escapeCharCodeInt tok =
  charCodeAsMadeToken <$> Lex.renderTokenAsCodes escapeCharCodeInt tok

-- For \number, \romannumeral, \string. \meaning, \jobname, and \fontname: Each
-- character code gets category "other" , except that 32 gets "space".
charCodeAsMadeToken :: Code.CharCode -> Lex.LexToken
charCodeAsMadeToken c =
  Lex.CharCatLexToken $
    Lex.LexCharCat c $ case c of
      Code.Chr_ ' ' -> Code.Space
      _ -> Code.Other

renderInternalQuantity :: Eval.InternalQuantity -> Seq Lex.LexToken
renderInternalQuantity = \case
  Eval.InternalIntQuantity n ->
    tokensFromInt n.unHexInt
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
    tokensFromInt :: Int -> Seq Lex.LexToken
    tokensFromInt n = tokensFromText (show n)

    tokensFromText :: Text -> Seq Lex.LexToken
    tokensFromText t = Seq.fromList $ charCodeAsMadeToken <$> (Code.textAsCharCodes t)
