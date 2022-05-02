module Hex.Stage.Parse.Impl.Parsers.Command.Assignment.Macro where

import Control.Monad.Combinators qualified as PC
import Data.Sequence qualified as Seq
import Hex.Common.Codes qualified as H.C
import Hex.Common.HexState.Interface.Resolve.PrimitiveToken qualified as T
import Hex.Common.HexState.Interface.Resolve.SyntaxToken qualified as T.Syn
import Hex.Common.Parse (MonadPrimTokenParse (..))
import Hex.Stage.Lex.Interface.Extract qualified as Lex
import Hex.Stage.Parse.Impl.Parsers.BalancedText qualified as Par
import Hex.Stage.Parse.Impl.Parsers.Combinators qualified as Par
import Hex.Stage.Parse.Interface.AST.Command qualified as AST
import Hexlude

parseMacroBody :: MonadPrimTokenParse m => T.ExpandDefFlag -> Seq T.AssignPrefixTok -> m AST.AssignmentBody
parseMacroBody defExpandType prefixes = do
  cs <- Par.parseCSName
  tgt <- parseMacroDefinition defExpandType prefixes
  pure $ AST.DefineControlSequence cs (AST.MacroTarget tgt)

parseParamText :: MonadPrimTokenParse m => m T.Syn.ParameterText
parseParamText = do
  lts <- PC.many $ do
    Par.satisfyLexThen $ \lt -> case lt ^? Lex.lexTokCategory of
      Just H.C.BeginGroup -> Nothing
      Just H.C.EndGroup -> Nothing
      _ -> Just lt
  pure $ T.Syn.ParameterText $ Seq.fromList lts

parseMacroDefinition :: MonadPrimTokenParse m => T.ExpandDefFlag -> Seq T.AssignPrefixTok -> m T.Syn.MacroDefinition
parseMacroDefinition defExpandType prefixes = do
  paramText <- parseParamText
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
