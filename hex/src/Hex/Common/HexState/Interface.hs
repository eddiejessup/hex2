{-# LANGUAGE AllowAmbiguousTypes #-}

module Hex.Common.HexState.Interface where

import Hex.Common.Codes qualified as Code
import Hex.Common.HexState.Interface.Code qualified as HSt.Code
import Hex.Common.HexState.Interface.Font qualified as Font
import Hex.Common.HexState.Interface.Grouped qualified as Grouped
import Hex.Common.HexState.Interface.Grouped qualified as HSt.Grouped
import Hex.Common.HexState.Interface.Parameter qualified as Param
import Hex.Common.HexState.Interface.Register qualified as Reg
import Hex.Common.HexState.Interface.Resolve qualified as HSt.Res
import Hex.Common.HexState.Interface.Variable qualified as Var
import Hex.Common.Quantity qualified as Q
import Hex.Common.Token.Lexed qualified as LT
import Hex.Common.Token.Resolved (ResolvedToken)
import Hex.Common.Token.Resolved qualified as RT
import Hex.Common.Token.Resolved.Primitive qualified as PT
import Hex.Stage.Build.BoxElem qualified as Box
import Hex.Stage.Build.BoxElem qualified as H.Inter.B.Box
import Hex.Stage.Build.ListElem qualified as H.Inter.B.List
import Hexlude
import qualified Formatting as F

data ResolutionError = UnknownSymbolError HSt.Res.ControlSymbol
  deriving stock (Show, Generic)

fmtResolutionError :: Fmt ResolutionError
fmtResolutionError = F.later $ \case
  UnknownSymbolError cSym ->
    "Got unknown control-symbol: " <> F.bformat HSt.Res.fmtControlSymbol cSym

class Monad m => MonadHexState m where
  getParameterValue :: Param.QuantParam q -> m (Var.QuantVariableTarget q)

  setParameterValue :: Param.QuantParam q -> Var.QuantVariableTarget q -> HSt.Grouped.ScopeFlag -> m ()

  getRegisterValue :: Reg.QuantRegisterLocation q -> m (Var.QuantVariableTarget q)

  setQuantRegisterValue :: Reg.QuantRegisterLocation q -> Var.QuantVariableTarget q -> HSt.Grouped.ScopeFlag -> m ()

  fetchBoxRegisterValue :: Reg.BoxFetchMode -> Reg.RegisterLocation -> m (Maybe (Box.Box Box.BaseBoxContents))

  setBoxRegisterValue :: Reg.RegisterLocation -> Maybe (Box.Box Box.BaseBoxContents) -> HSt.Grouped.ScopeFlag -> m ()

  getSpecialIntParameter :: Param.SpecialIntParameter -> m Q.HexInt

  getSpecialLengthParameter :: Param.SpecialLengthParameter -> m Q.Length

  setSpecialIntParameter :: Param.SpecialIntParameter -> Q.HexInt -> m ()

  setSpecialLengthParameter :: Param.SpecialLengthParameter -> Q.Length -> m ()

  getHexCode :: Code.CCodeType c -> Code.CharCode -> m (HSt.Code.CodeTableTarget c)

  setHexCode :: Code.CCodeType c -> Code.CharCode -> HSt.Code.CodeTableTarget c -> HSt.Grouped.ScopeFlag -> m ()

  resolveSymbol :: HSt.Res.ControlSymbol -> m (Maybe ResolvedToken)

  setSymbol :: HSt.Res.ControlSymbol -> ResolvedToken -> HSt.Grouped.ScopeFlag -> m ()

  loadFont :: Q.HexFilePath -> H.Inter.B.Box.FontSpecification -> m H.Inter.B.Box.FontDefinition

  selectFont :: Font.FontNumber -> HSt.Grouped.ScopeFlag -> m ()

  setFamilyMemberFont :: Font.FamilyMember -> Font.FontNumber -> HSt.Grouped.ScopeFlag -> m ()

  currentFontNumber :: m Font.FontNumber

  setFontSpecialCharacter :: Font.FontSpecialChar -> Font.FontNumber -> Q.HexInt -> m ()

  currentFontCharacter :: Code.CharCode -> m (Maybe (Q.Length, Q.Length, Q.Length, Q.Length))

  currentFontSpaceGlue :: m (Maybe Q.Glue)

  popAfterAssignmentToken :: m (Maybe LT.LexToken)

  setAfterAssignmentToken :: LT.LexToken -> m ()

  pushGroup :: Maybe Grouped.ScopedGroupType -> m ()

  popGroup :: Grouped.ChangeGroupTrigger -> m HSt.Grouped.HexGroupType

instance MonadHexState m => MonadHexState (StateT a m) where
  getParameterValue x = lift $ getParameterValue x
  setParameterValue x y z = lift $ setParameterValue x y z
  getRegisterValue x = lift $ getRegisterValue x
  setQuantRegisterValue x y z = lift $ setQuantRegisterValue x y z
  fetchBoxRegisterValue x y = lift $ fetchBoxRegisterValue x y
  setBoxRegisterValue x y z = lift $ setBoxRegisterValue x y z
  getSpecialIntParameter x = lift $ getSpecialIntParameter x
  getSpecialLengthParameter x = lift $ getSpecialLengthParameter x
  setSpecialIntParameter x y = lift $ setSpecialIntParameter x y
  setSpecialLengthParameter x y = lift $ setSpecialLengthParameter x y
  getHexCode x y = lift $ getHexCode x y
  setHexCode w x y z = lift $ setHexCode w x y z
  resolveSymbol x = lift $ resolveSymbol x
  loadFont x y = lift $ loadFont x y
  selectFont x y = lift $ selectFont x y
  setFamilyMemberFont x y z = lift $ setFamilyMemberFont x y z
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
  HSt.Grouped.ScopeFlag ->
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
  HSt.Grouped.ScopeFlag ->
  m ()
modifyRegisterValue loc f scopeFlag = do
  currentVal <- getRegisterValue loc
  setQuantRegisterValue loc (f currentVal) scopeFlag

advanceParameterValue ::
  forall q m.
  (MonadHexState m, Semigroup (Var.QuantVariableTarget q)) =>
  Param.QuantParam q ->
  Var.QuantVariableTarget q ->
  HSt.Grouped.ScopeFlag ->
  m ()
advanceParameterValue p plusVal =
  modifyParameterValue @q p (\v -> v <> plusVal)

scaleParameterValue ::
  forall q m.
  (MonadHexState m, Q.Scalable (Var.QuantVariableTarget q)) =>
  Param.QuantParam q ->
  Q.VDirection ->
  Q.HexInt ->
  HSt.Grouped.ScopeFlag ->
  m ()
scaleParameterValue p scaleDirection arg scopeFlag =
  modifyParameterValue @q p (Q.scaleInDirection scaleDirection arg) scopeFlag

advanceRegisterValue ::
  (MonadHexState m, Semigroup (Var.QuantVariableTarget q)) =>
  Reg.QuantRegisterLocation q ->
  Var.QuantVariableTarget q ->
  HSt.Grouped.ScopeFlag ->
  m ()
advanceRegisterValue loc plusVal =
  modifyRegisterValue loc (\v -> v <> plusVal)

scaleRegisterValue ::
  (MonadHexState m, Q.Scalable (Var.QuantVariableTarget q)) =>
  Reg.QuantRegisterLocation q ->
  Q.VDirection ->
  Q.HexInt ->
  HSt.Grouped.ScopeFlag ->
  m ()
scaleRegisterValue qLoc scaleDirection arg scopeFlag =
  modifyRegisterValue qLoc (Q.scaleInDirection scaleDirection arg) scopeFlag


resolveLexToken ::
  MonadHexState m =>
  LT.LexToken ->
  m (Either ResolutionError RT.ResolvedToken)
resolveLexToken = \case
  LT.ControlSequenceLexToken cs -> do
    resolveSymbolWithError (HSt.Res.ControlSequenceSymbol cs)
  LT.CharCatLexToken (LT.LexCharCat c Code.Active) ->
    resolveSymbolWithError $ HSt.Res.ActiveCharacterSymbol c
  LT.CharCatLexToken lexCharCat ->
    pure $ Right $ RT.PrimitiveToken $ PT.CharCatPair lexCharCat
  where
    resolveSymbolWithError cSym = resolveSymbol cSym >>= \case
      Nothing -> pure $ Left $ UnknownSymbolError cSym
      Just rt -> pure $ Right rt
