module Hex.Parse.Parsers.Command.Assignment.Macro where

import Control.Monad.Combinators qualified as PC
import Data.Sequence qualified as Seq
import Hex.Codes qualified as H.C
import Hex.Lex.Types qualified as H.Lex
import Hex.Parse.AST.Command qualified as AST
import Hex.Parse.MonadPrimTokenSource.Interface
import Hex.Parse.Parsers.BalancedText qualified as Par
import Hex.Parse.Parsers.Combinators qualified as Par
import Hex.Symbol.Token.Primitive qualified as T
import Hex.Symbol.Token.SyntaxCommandHead qualified as T.Syn
import Hexlude

parseMacroBody :: MonadPrimTokenSource m => T.ExpandDefFlag -> Seq T.AssignPrefixTok -> m AST.AssignmentBody
parseMacroBody defExpandType prefixes = do
  cs <- Par.parseCSName
  tgt <- parseMacroDefinition defExpandType prefixes
  pure $ AST.DefineControlSequence cs (AST.MacroTarget tgt)

inhibParseParamText :: MonadPrimTokenSource m => Par.InhibitionToken -> m T.Syn.ParameterText
inhibParseParamText inhibToken = do
  lts <- PC.many $ do
    Par.inhibSatisfyLexThen inhibToken $ \lt -> case lt ^? H.Lex.lexTokCategory of
      Just H.C.BeginGroup -> Nothing
      Just H.C.EndGroup -> Nothing
      _ -> Just lt
  pure $ T.Syn.ParameterText $ Seq.fromList lts

parseMacroDefinition :: MonadPrimTokenSource m => T.ExpandDefFlag -> Seq T.AssignPrefixTok -> m T.Syn.MacroDefinition
parseMacroDefinition defExpandType prefixes = do
  paramText <- Par.withInhibition inhibParseParamText
  replacementText <- case defExpandType of
    T.ExpandDef ->
      T.Syn.ExpandedReplacementText <$> Par.parseExpandedBalancedText
    T.InhibitDef -> do
      T.Syn.InhibitedReplacementText <$> Par.parseInhibitedBalancedText
  pure
    T.Syn.MacroDefinition
      { T.Syn.paramText,
        T.Syn.replacementText,
        T.Syn.long = T.LongTok `elem` prefixes,
        T.Syn.outer = T.OuterTok `elem` prefixes
      }
