module Hex.Stage.Parse.Impl.Parsers.ExpansionCommand.MacroCall where

import Control.Monad.Combinators qualified as PC
import Data.Sequence qualified as Seq
import Hex.Capability.Log.Interface qualified as Log
import Hex.Common.Codes qualified as Code
import Hex.Common.HexState.Interface.TokenList qualified as HSt.LT
import Hex.Common.HexState.Interface.TokenList qualified as HSt.TL
import Hex.Common.Token.Lexed qualified as LT
import Hex.Common.Token.Resolved.Expandable qualified as ST
import Hex.Stage.Expand.Interface (PrimTokenSource (..))
import Hex.Stage.Parse.Impl.Parsers.BalancedText qualified as Par
import Hex.Stage.Parse.Impl.Parsers.Combinators
import Hex.Stage.Parse.Interface.AST.ExpansionCommand qualified as AST
import Hexlude

-- During a macro call, we must parse the provided arguments.
parseMacroArguments :: forall es. [PrimTokenSource, EAlternative, Log.HexLog] :>> es => ST.MacroParameterSpecification -> Eff es AST.MacroArgumentList
parseMacroArguments parameterSpec = do
  skipParameterText parameterSpec.preParameterText
  AST.MacroArgumentList <$> parseArguments parameterSpec.parameterDelimiterTexts
  where
    parseArguments :: Seq ST.ParameterText -> Eff es (Seq AST.MacroArgument)
    parseArguments = \case
      -- If there are no parameters, expect no arguments.
      Seq.Empty -> pure Seq.Empty
      p :<| rest -> do
        -- Get the raw argument body, before any stripping.
        arg <-
          case p of
            -- If the post-parameter delimiter text is empty, parse the
            -- arguments without expecting any delimiter text.
            ST.ParameterText Empty -> parseUndelimitedArgumentTokens
            -- If the post-parameter delimiter text is not empty, parse the
            -- arguments expecting that delimiter text.
            ST.ParameterText delims -> do
              argText <- parseDelimitedArgumentTokens delims
              -- If the argument has the form ‘{⟨nested tokens⟩}’, where ⟨nested
              -- tokens⟩ stands for any properly nested token sequence, the outermost
              -- braces are removed.
              -- If appropriate, strip the argument; otherwise use the unstripped
              -- version.
              pure $ stripOuterBracePairIfPresent argText
        argsRest <- parseArguments rest
        pure $ (AST.MacroArgument arg) <| argsRest

    -- Check if an argument has an outer '{}' pair that should be stripped, and
    -- do this if so.
    -- If we got an empty argument, can consider this to 'strip' to itself.
    stripOuterBracePairIfPresent :: HSt.TL.InhibitedBalancedText -> HSt.TL.InhibitedBalancedText
    stripOuterBracePairIfPresent original@(HSt.TL.InhibitedBalancedText (HSt.LT.BalancedText outer)) = case outer of
      -- Must have at least 2 tokens.
      -- First token must be a '{'.
      -- The last token must be a '}'.
      -- The stripped argument must have valid grouping.
      a :<| (inner :|> z)
        | lexTokenHasCategory Code.BeginGroup a
            && lexTokenHasCategory Code.EndGroup z
            && hasValidGrouping inner ->
            HSt.TL.InhibitedBalancedText $ HSt.LT.BalancedText inner
      _ ->
        original

-- If the parameter is undelimited, the argument is the next non-blank
-- token, unless that token is ‘{’, when the argument will be the entire
-- following {...} group.
parseUndelimitedArgumentTokens :: [PrimTokenSource, EAlternative, Log.HexLog] :>> es => Eff es HSt.TL.InhibitedBalancedText
parseUndelimitedArgumentTokens = do
  -- Skip blank tokens (assumed to mean spaces).
  PC.skipMany $ satisfyIf (satisfyCharCatThen Inhibited) (charCatHasCategory Code.Space)
  anyLexInhibited >>= \case
    -- Note that we are throwing away the surrounding braces of the argument.
    LT.CharCatLexToken LT.LexCharCat {lexCCCat = Code.BeginGroup} ->
      Par.parseInhibitedBalancedText Par.AlreadySeenBeginGroup
    t ->
      pure $ HSt.TL.InhibitedBalancedText $ HSt.LT.BalancedText (singleton t)

-- Get the shortest, possibly empty, properly nested sequence of tokens,
-- followed by the delimiter tokens. In the delimiter, category codes,
-- character codes and control sequence names must match.
parseDelimitedArgumentTokens :: forall es. [PrimTokenSource, EAlternative, Log.HexLog] :>> es => Seq LT.LexToken -> Eff es HSt.TL.InhibitedBalancedText
parseDelimitedArgumentTokens delims = go Empty
  where
    -- Get the shortest, possibly empty, properly nested sequence of tokens,
    -- followed by the delimiter tokens. In the delimiter, category codes,
    -- character codes and control sequence names must match.
    go :: Seq LT.LexToken -> Eff es HSt.TL.InhibitedBalancedText
    go argTokensAccum = do
      -- Parse tokens until we see the delimiter tokens, then add what we grab
      -- to our accumulating argument.
      newArgTokens <- Seq.fromList <$> PC.manyTill anyLexInhibited (skipUnexpandedLexTokens delims)
      -- Consider the new 'total sequence' of tokens.
      let argTokensNew = argTokensAccum <> newArgTokens
      -- Check if that new sequence is a valid group.
      if hasValidGrouping argTokensNew
        then -- If the argument has valid grouping, then we are done, that is the
        -- argument.
          pure $ HSt.TL.InhibitedBalancedText $ HSt.LT.BalancedText argTokensNew
        else -- Otherwise, add the 'red herring' delimiters we just parsed and
        -- continue extending the argument token-sequence.
          go (argTokensNew <> delims)

-- | Parse a sequence of lex-tokens, asserting that the result matched the
-- contents of a parameter-text.
skipParameterText :: [PrimTokenSource, EAlternative, Log.HexLog] :>> es => ST.ParameterText -> Eff es ()
skipParameterText (ST.ParameterText lexTokens) = skipUnexpandedLexTokens lexTokens

skipUnexpandedLexTokens :: [PrimTokenSource, EAlternative, Log.HexLog] :>> es => Seq LT.LexToken -> Eff es ()
skipUnexpandedLexTokens ts = forM_ ts (satisfyLexEquals Inhibited)

-- For some expression consisting of tokens that might increase or decrease the grouping, such as a parentheses,
-- Compute the final depth of the expression, and the number of matched groups we saw.
nrExpressions :: Seq LT.LexToken -> Maybe (Int, Int)
nrExpressions = foldM next (0, 0)
  where
    next v@(dpth, nrExprs) t
      -- Consider the current token, and whether it increases the expression
      -- depth, decreases it, or leaves it unchanged.
      -- If we enter a group, then increase our current depth, but
      -- leave our expression-count unchanged.
      | lexTokenHasCategory Code.BeginGroup t =
          Just (succ dpth, nrExprs)
      -- If we finish a group, consider our new depth.
      | lexTokenHasCategory Code.EndGroup t =
          let newDepth = pred dpth
           in if
                  -- The only way we could see a depth below zero is if we see a
                  -- close-bracket with no earlier matching open-bracket. This means the
                  -- expression is invalid.
                  | newDepth < 0 -> Nothing
                  | newDepth == 0 -> Just (newDepth, succ nrExprs)
                  | otherwise -> Just (newDepth, nrExprs)
      -- If no change, then just continue with the previous value.
      | otherwise = Just v

-- | Whether some sequence of lex-tokens is a valid balanced-text.
-- This is equivalent to asking whether we can traverse the token-list, tracking
-- our depth, and eventually arrive at zero without ever going negative.
hasValidGrouping :: Seq LT.LexToken -> Bool
hasValidGrouping ts = case nrExpressions ts of
  Just (0, _) -> True
  _ -> False
