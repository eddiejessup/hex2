module Hex.Stage.Evaluate.Impl.SyntaxCommand where

import Hex.Common.HexState.Interface qualified as HSt
import Hex.Stage.Evaluate.Impl.Common qualified as Eval
import Hex.Stage.Evaluate.Impl.Quantity qualified as Eval
import Hex.Stage.Evaluate.Impl.SyntaxCommand.Condition qualified as Eval
import Hex.Stage.Evaluate.Interface.AST.SyntaxCommand qualified as E
import Hex.Stage.Parse.Interface.AST.SyntaxCommand qualified as P
import Hexlude

evalSyntaxCommand ::
  ( MonadError e m,
    AsType Eval.EvaluationError e,
    HSt.MonadHexState m
  ) =>
  P.SyntaxCommand ->
  m E.SyntaxCommand
evalSyntaxCommand = \case
  P.CallMacro macroDefinition macroArgumentList ->
    pure $ E.CallMacro macroDefinition macroArgumentList
  P.ApplyConditionHead conditionHead ->
    E.ApplyConditionHead <$> Eval.evalConditionHead conditionHead
  P.ApplyConditionBody conditionBodyTok ->
    pure $ E.ApplyConditionBody conditionBodyTok
  P.RenderNumber n ->
    E.RenderNumber <$> Eval.evalInt n
  P.RenderRomanNumeral n ->
    E.RenderRomanNumeral <$> Eval.evalInt n
  P.RenderTokenAsTokens lexToken ->
    pure $ E.RenderTokenAsTokens lexToken
  P.RenderJobName ->
    pure $ E.RenderJobName
  P.RenderFontName fontRef ->
    pure $ E.RenderFontName fontRef
  P.RenderTokenMeaning lexToken ->
    pure $ E.RenderTokenMeaning lexToken
  P.ParseControlSequence cs ->
    pure $ E.ParseControlSequence cs
  P.ExpandAfter noExpandLexToken toExpandLexToken ->
    pure $ E.ExpandAfter noExpandLexToken toExpandLexToken
  P.NoExpand lexToken ->
    pure $ E.NoExpand lexToken
  P.GetMarkRegister markRegister ->
    pure $ E.GetMarkRegister markRegister
  P.OpenInputFile filePath ->
    pure $ E.OpenInputFile filePath
  P.EndInputFile ->
    pure $ E.EndInputFile
  P.RenderInternalQuantity internalQuantity ->
    E.RenderInternalQuantity <$> Eval.evalInternalQuantity internalQuantity
  P.ChangeCase vDirection inhibText ->
    pure $ E.ChangeCase vDirection inhibText
