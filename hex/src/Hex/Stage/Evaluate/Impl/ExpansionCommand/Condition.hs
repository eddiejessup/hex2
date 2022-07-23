module Hex.Stage.Evaluate.Impl.ExpansionCommand.Condition where

import Hex.Common.HexState.Interface qualified as HSt
import Hex.Common.HexState.Interface.Resolve qualified as Res
import Hex.Common.Quantity qualified as Q
import Hex.Common.Token.Lexed qualified as LT
import Hex.Common.Token.Resolved.Expandable qualified as ST
import Hex.Common.Token.Resolved.Primitive qualified as PT
import Hex.Stage.Evaluate.Impl.Common qualified as Eval
import Hex.Stage.Evaluate.Impl.Quantity qualified as Eval
import Hex.Stage.Evaluate.Interface.AST.ExpansionCommand qualified as E
import Hex.Stage.Parse.Interface.AST.ExpansionCommand qualified as P
import Hexlude
import Hex.Common.HexState.Interface (EHexState)

evalConditionHead :: [Error Eval.EvaluationError, EHexState] :>> es => P.ConditionHead -> Eff es E.ConditionOutcome
evalConditionHead = \case
  P.IfConditionHead ifConditionHead -> do
    evalHead <- evalIfConditionHeadToEvalHead ifConditionHead
    evalIfConditionHeadToBool evalHead <&> \case
      True -> E.IfConditionOutcome E.SkipElseBlock
      False -> E.IfConditionOutcome E.SkipPreElseBlock
  P.CaseConditionHead caseNr ->
    E.CaseConditionOutcome <$> Eval.evalInt caseNr

evalIfConditionHeadToEvalHead :: [Error Eval.EvaluationError, EHexState] :>> es => P.IfConditionHead -> Eff es E.IfConditionHead
evalIfConditionHeadToEvalHead = \case
  P.IfIntPairTest lhs ordering rhs ->
    E.IfIntPairTest <$> Eval.evalInt lhs <*> pure ordering <*> Eval.evalInt rhs
  P.IfLengthPairTest lhs ordering rhs ->
    E.IfLengthPairTest <$> Eval.evalLength lhs <*> pure ordering <*> Eval.evalLength rhs
  P.IfIntOdd n ->
    E.IfIntOdd <$> Eval.evalInt n
  P.IfInMode modeAttribute ->
    pure $ E.IfInMode modeAttribute
  P.IfTokenAttributesEqual tokenAttribute lt1 lt2 ->
    pure $ E.IfTokenAttributesEqual tokenAttribute lt1 lt2
  P.IfTokensEqual lt1 lt2 ->
    pure $ E.IfTokensEqual lt1 lt2
  P.IfBoxRegisterIs boxRegisterAttribute n ->
    E.IfBoxRegisterIs boxRegisterAttribute <$> Eval.evalInt n
  P.IfInputEnded n ->
    E.IfInputEnded <$> Eval.evalInt n
  P.IfConst constBool ->
    pure $ E.IfConst constBool

evalIfConditionHeadToBool :: EHexState :> es => E.IfConditionHead -> Eff es Bool
evalIfConditionHeadToBool = \case
  E.IfIntPairTest lhs ordering rhs ->
    pure $ ordToComp ordering lhs rhs
  E.IfLengthPairTest lhs ordering rhs ->
    pure $ ordToComp ordering lhs rhs
  E.IfIntOdd n ->
    pure $ odd n.unHexInt
  E.IfInMode _ ->
    notImplemented "IfInMode"
  -- A control sequence token is considered to have character code 256 and
  -- category code 16.
  -- This logic is hard to follow literally, because my category code type
  -- is an explicit enumeration, not an integer. So I'll just interpret it
  -- as: control sequences are considered equal to each other, and unequal to
  -- all char-cat pairs.
  -- TODO: Unless the control sequence has been \let equal to a non-active
  -- character token.
  E.IfTokenAttributesEqual ST.CharCodeAttribute t1 t2 ->
    pure $ eqChars t1 t2
  E.IfTokenAttributesEqual ST.CatCodeAttribute t1 t2 ->
    pure $ eqCats t1 t2
  --  The condition is true if (a) the two tokens are not macros, and they
  --  both represent the same (character code, category code) pair, the same
  --  TeX primitive, the same \font or \chardef or \countdef, etc.; or if (b)
  --  the two tokens are macros, and they both have the same status with
  --  respect to \long and \outer, and they both have the same parameters and
  --  “top level” expansion.
  E.IfTokensEqual (LT.CharCatLexToken cc1) (LT.CharCatLexToken cc2) ->
    pure $ cc1 == cc2
  E.IfTokensEqual (LT.ControlSequenceLexToken cs1) (LT.ControlSequenceLexToken cs2) -> do
    -- Surprisingly, two undefined control sequences are considered equal,
    -- so we may compare the Maybe types.
    symbol1 <- HSt.resolveSymbol (Res.ControlSequenceSymbol cs1)
    symbol2 <- HSt.resolveSymbol (Res.ControlSequenceSymbol cs2)
    pure $ symbol1 == symbol2
  E.IfTokensEqual (LT.CharCatLexToken _) (LT.ControlSequenceLexToken _) ->
    pure False
  E.IfTokensEqual (LT.ControlSequenceLexToken _) (LT.CharCatLexToken _) ->
    pure False
  E.IfBoxRegisterIs attr _n -> do
    case attr of
      PT.HasVerticalBox ->
        notImplemented "Evaluate IfBoxRegister HasVerticalBox to bool"
      PT.HasHorizontalBox ->
        notImplemented "Evaluate IfBoxRegister HasHorizontalBox to bool"
      PT.IsVoid ->
        notImplemented "Evaluate IfBoxRegister IsVoid to bool"
  E.IfInputEnded _n -> do
    notImplemented "Evaluate IfInputEnded to bool"
  E.IfConst b -> pure b
  where
    ordToComp GT = (>)
    ordToComp LT = (<)
    ordToComp EQ = (==)

    eqChars
      (LT.CharCatLexToken (LT.LexCharCat c1 _))
      (LT.CharCatLexToken (LT.LexCharCat c2 _)) = c1 == c2
    eqChars _ _ = True

    eqCats
      (LT.CharCatLexToken (LT.LexCharCat _ c1))
      (LT.CharCatLexToken (LT.LexCharCat _ c2)) = c1 == c2
    eqCats _ _ = True
