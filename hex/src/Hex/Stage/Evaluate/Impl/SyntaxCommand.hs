module Hex.Stage.Evaluate.Impl.SyntaxCommand where

import Hex.Common.HexState.Interface qualified as HSt
import Hex.Stage.Evaluate.Impl.Common qualified as Eval
import Hex.Stage.Evaluate.Interface.AST.SyntaxCommand qualified as E
import Hex.Stage.Parse.Interface.AST.SyntaxCommand qualified as P
import Hexlude
import qualified Hex.Stage.Evaluate.Impl.SyntaxCommand.Condition as Eval
import qualified Hex.Stage.Evaluate.Impl.Quantity as Eval

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
  P.RenderTokenAsString lexToken ->
    pure $ E.RenderTokenAsString lexToken
  P.RenderJobName ->
    pure $ E.RenderJobName
  P.RenderFontName fontRef ->
    pure $ E.RenderFontName fontRef
  P.RenderTokenMeaning lexToken ->
    pure $ E.RenderTokenMeaning lexToken
  P.ParseControlSequence bytes ->
    pure $ E.ParseControlSequence bytes
  P.ExpandAfter lexToken ->
    pure $ E.ExpandAfter lexToken
  P.NoExpand lexToken ->
    pure $ E.NoExpand lexToken
  P.GetMarkRegister markRegister ->
    pure $ E.GetMarkRegister markRegister
  P.OpenInputFile filePath ->
    pure $ E.OpenInputFile filePath
  P.EndInputFile ->
    pure $ E.EndInputFile
  P.RenderInternalQuantity ->
    pure $ E.RenderInternalQuantity
  P.ChangeCase vDirection ->
    pure $ E.ChangeCase vDirection
