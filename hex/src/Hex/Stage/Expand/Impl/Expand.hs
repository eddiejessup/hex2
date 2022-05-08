module Hex.Stage.Expand.Impl.Expand where

import Data.Sequence qualified as Seq
import Hex.Common.HexState.Interface.Resolve.SyntaxToken qualified as ST
import Hex.Stage.Expand.Interface (ExpansionError)
import Hex.Stage.Expand.Interface qualified as Expand
import Hex.Stage.Lex.Interface.Extract qualified as Lex
import Hexlude

substituteArgsIntoMacroBody ::
  forall m e.
  ( MonadError e m,
    AsType ExpansionError e
  ) =>
  ST.MacroReplacementText ->
  Expand.MacroArgumentList ->
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
    -- | The contents of a macro replacement text is either:
    -- - An ordinary lex-token. In this case the output is just that lex-token itself.
    -- - A reference to some macro parameter. In this case we should look up the
    --   relevant argument in the argument-list, and that is our output.
    --   If the macro refers to a parameter that isn't present in our argument
    --   list, then the user didn't provide enough arguments.
    renderToken :: ST.MacroTextToken -> m (Seq Lex.LexToken)
    renderToken = \case
      ST.MacroTextLexToken x ->
        pure (Seq.singleton x)
      ST.MacroTextParamToken argIx -> case Expand.lookupArg argIx argsList of
        Nothing ->
          throwError $ injectTyped $ Expand.MacroArgumentSubstitutionError argIx argsList
        Just arg ->
          pure $ arg.unMacroArgument.unInhibitedBalancedText
