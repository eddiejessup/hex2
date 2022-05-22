{-# LANGUAGE AllowAmbiguousTypes #-}

module Hex.Common.HexState.Interface where

import Hex.Common.Codes qualified as Code
import Hex.Common.HexState.Interface.Code qualified as HSt.Code
import Hex.Common.HexState.Interface.Grouped qualified as Grouped
import Hex.Common.HexState.Interface.Parameter qualified as Param
import Hex.Common.HexState.Interface.Register qualified as Reg
import Hex.Common.HexState.Interface.Resolve (ControlSymbol, ResolvedToken)
import Hex.Common.HexState.Interface.Resolve.PrimitiveToken qualified as PT
import Hex.Common.HexState.Interface.Variable qualified as Var
import Hex.Common.Quantity qualified as Q
import Hex.Stage.Interpret.Build.Box.Elem qualified as H.Inter.B.Box
import Hex.Stage.Interpret.Build.List.Elem qualified as H.Inter.B.List
import Hex.Stage.Lex.Interface.Extract qualified as Lex
import Hexlude

class Monad m => MonadHexState m where
  getParameterValue :: Param.QuantParam q -> m (Var.QuantVariableTarget q)

  setParameterValue :: Param.QuantParam q -> Var.QuantVariableTarget q -> PT.ScopeFlag -> m ()

  getRegisterValue :: Reg.QuantRegisterLocation q -> m (Var.QuantVariableTarget q)

  setRegisterValue :: Reg.QuantRegisterLocation q -> Var.QuantVariableTarget q -> PT.ScopeFlag -> m ()

  getSpecialIntParameter :: Param.SpecialIntParameter -> m Q.HexInt

  getSpecialLengthParameter :: Param.SpecialLengthParameter -> m Q.Length

  setSpecialIntParameter :: Param.SpecialIntParameter -> Q.HexInt -> m ()

  setSpecialLengthParameter :: Param.SpecialLengthParameter -> Q.Length -> m ()

  getHexCode :: Code.CCodeType c -> Code.CharCode -> m (HSt.Code.CodeTableTarget c)

  setHexCode :: Code.CCodeType c -> Code.CharCode -> HSt.Code.CodeTableTarget c -> PT.ScopeFlag -> m ()

  resolveSymbol :: ControlSymbol -> m (Maybe ResolvedToken)

  setSymbol :: ControlSymbol -> ResolvedToken -> PT.ScopeFlag -> m ()

  loadFont :: Q.HexFilePath -> H.Inter.B.Box.FontSpecification -> m H.Inter.B.Box.FontDefinition

  selectFont :: PT.FontNumber -> PT.ScopeFlag -> m ()

  currentFontNumber :: m PT.FontNumber

  setFontSpecialCharacter :: PT.FontSpecialChar -> PT.FontNumber -> Q.HexInt -> m ()

  currentFontCharacter :: Code.CharCode -> m (Maybe (Q.Length, Q.Length, Q.Length, Q.Length))

  currentFontSpaceGlue :: m (Maybe Q.Glue)

  popAfterAssignmentToken :: m (Maybe Lex.LexToken)

  setAfterAssignmentToken :: Lex.LexToken -> m ()

  pushGroup :: Maybe Grouped.ScopedGroupType -> m ()

  popGroup :: Grouped.LocalStructureTrigger -> m ()

instance MonadHexState m => MonadHexState (StateT a m) where
  getParameterValue x = lift $ getParameterValue x
  setParameterValue x y z = lift $ setParameterValue x y z
  getRegisterValue x = lift $ getRegisterValue x
  setRegisterValue x y z = lift $ setRegisterValue x y z
  getSpecialIntParameter x = lift $ getSpecialIntParameter x
  getSpecialLengthParameter x = lift $ getSpecialLengthParameter x
  setSpecialIntParameter x y = lift $ setSpecialIntParameter x y
  setSpecialLengthParameter x y = lift $ setSpecialLengthParameter x y
  getHexCode x y = lift $ getHexCode x y
  setHexCode w x y z = lift $ setHexCode w x y z
  resolveSymbol x = lift $ resolveSymbol x
  loadFont x y = lift $ loadFont x y
  selectFont x y = lift $ selectFont x y
  setFontSpecialCharacter x y z = lift $ setFontSpecialCharacter x y z
  currentFontNumber = lift currentFontNumber
  currentFontCharacter x = lift $ currentFontCharacter x
  currentFontSpaceGlue = lift currentFontSpaceGlue
  popAfterAssignmentToken = lift popAfterAssignmentToken
  setAfterAssignmentToken x = lift $ setAfterAssignmentToken x
  setSymbol x y z = lift $ setSymbol x y z
  pushGroup x = lift $ pushGroup x
  popGroup x = lift $ popGroup x

getParIndentBox :: MonadHexState m => m H.Inter.B.List.HListElem
getParIndentBox = do
  boxWidth <- getParameterValue (Param.LengthQuantParam Param.ParIndent)
  pure $
    H.Inter.B.List.HVListElem $
      H.Inter.B.List.VListBaseElem $
        H.Inter.B.Box.ElemBox $
          H.Inter.B.Box.Box
            { H.Inter.B.Box.contents = H.Inter.B.Box.HBoxContents (H.Inter.B.Box.HBoxElemSeq Empty),
              H.Inter.B.Box.boxWidth,
              H.Inter.B.Box.boxHeight = mempty,
              H.Inter.B.Box.boxDepth = mempty
            }

modifyParameterValue ::
  forall (q :: Q.QuantityType) m.
  MonadHexState m =>
  Param.QuantParam q ->
  ( Var.QuantVariableTarget q ->
    Var.QuantVariableTarget q
  ) ->
  PT.ScopeFlag ->
  m ()
modifyParameterValue p f scopeFlag = do
  currentVal <- getParameterValue @_ @q p
  setParameterValue p (f currentVal) scopeFlag

modifyRegisterValue ::
  MonadHexState m =>
  Reg.QuantRegisterLocation q ->
  ( Var.QuantVariableTarget q ->
    Var.QuantVariableTarget q
  ) ->
  PT.ScopeFlag ->
  m ()
modifyRegisterValue loc f scopeFlag = do
  currentVal <- getRegisterValue loc
  setRegisterValue loc (f currentVal) scopeFlag

advanceParameterValue ::
  forall q m.
  (MonadHexState m, Semigroup (Var.QuantVariableTarget q)) =>
  Param.QuantParam q ->
  Var.QuantVariableTarget q ->
  PT.ScopeFlag ->
  m ()
advanceParameterValue p plusVal =
  modifyParameterValue @q p (\v -> v <> plusVal)

scaleParameterValue ::
  forall q m.
  (MonadHexState m, Q.Scalable (Var.QuantVariableTarget q)) =>
  Param.QuantParam q ->
  Q.VDirection ->
  Q.HexInt ->
  PT.ScopeFlag ->
  m ()
scaleParameterValue p scaleDirection arg scopeFlag =
  modifyParameterValue @q p (Q.scaleInDirection scaleDirection arg) scopeFlag

advanceRegisterValue ::
  (MonadHexState m, Semigroup (Var.QuantVariableTarget q)) =>
  Reg.QuantRegisterLocation q ->
  Var.QuantVariableTarget q ->
  PT.ScopeFlag ->
  m ()
advanceRegisterValue loc plusVal =
  modifyRegisterValue loc (\v -> v <> plusVal)

scaleRegisterValue ::
  (MonadHexState m, Q.Scalable (Var.QuantVariableTarget q)) =>
  Reg.QuantRegisterLocation q ->
  Q.VDirection ->
  Q.HexInt ->
  PT.ScopeFlag ->
  m ()
scaleRegisterValue qLoc scaleDirection arg scopeFlag =
  modifyRegisterValue qLoc (Q.scaleInDirection scaleDirection arg) scopeFlag
