{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE TemplateHaskell #-}

module Hex.Common.HexState.Interface where

import Formatting qualified as F
import Hex.Common.Box qualified as Box
import Hex.Common.Codes qualified as Code
import Hex.Common.HexState.Interface.Code qualified as HSt.Code
import Hex.Common.HexState.Interface.Font qualified as Font
import Hex.Common.HexState.Interface.Grouped qualified as Grouped
import Hex.Common.HexState.Interface.Grouped qualified as HSt.Grouped
import Hex.Common.HexState.Interface.Mode qualified as HSt.Mode
import Hex.Common.HexState.Interface.Parameter qualified as Param
import Hex.Common.HexState.Interface.Register qualified as Reg
import Hex.Common.HexState.Interface.Resolve qualified as HSt.Res
import Hex.Common.HexState.Interface.Variable qualified as Var
import Hex.Common.Quantity qualified as Q
import Hex.Common.TFM.Types qualified as TFM
import Hex.Common.Token.Lexed qualified as LT
import Hex.Common.Token.Resolved (ResolvedToken)
import Hex.Common.Token.Resolved qualified as RT
import Hex.Common.Token.Resolved.Primitive qualified as PT
import Hex.Stage.Build.BoxElem qualified as BoxElem
import Hex.Stage.Build.ListElem qualified as ListElem
import Hex.Stage.Render.Interface.DocInstruction qualified as DVI
import Hexlude

data ResolutionError = UnknownSymbolError HSt.Res.ControlSymbol
  deriving stock (Show, Generic)

fmtResolutionError :: Fmt ResolutionError
fmtResolutionError = F.later $ \case
  UnknownSymbolError cSym ->
    "Got unknown control-symbol: " <> F.bformat HSt.Res.fmtControlSymbol cSym

data CharacterAttrs = CharacterAttrs
  { width, height, depth, italicCorrection :: Q.Length
  }
  deriving stock (Show, Generic)

data EHexState :: Effect where
  GetParameterValue :: Param.QuantParam q -> EHexState m (Var.QuantVariableTarget q)
  SetParameterValue :: Param.QuantParam q -> Var.QuantVariableTarget q -> HSt.Grouped.ScopeFlag -> EHexState m ()
  GetRegisterValue :: Reg.QuantRegisterLocation q -> EHexState m (Var.QuantVariableTarget q)
  SetQuantRegisterValue :: Reg.QuantRegisterLocation q -> Var.QuantVariableTarget q -> HSt.Grouped.ScopeFlag -> EHexState m ()
  FetchBoxRegisterValue :: Reg.BoxFetchMode -> Reg.RegisterLocation -> EHexState m (Maybe BoxElem.BaseBox)
  SetBoxRegisterValue :: Reg.RegisterLocation -> Maybe BoxElem.BaseBox -> HSt.Grouped.ScopeFlag -> EHexState m ()
  GetSpecialIntParameter :: Param.SpecialIntParameter -> EHexState m Q.HexInt
  GetSpecialLengthParameter :: Param.SpecialLengthParameter -> EHexState m Q.Length
  SetSpecialIntParameter :: Param.SpecialIntParameter -> Q.HexInt -> EHexState m ()
  SetSpecialLengthParameter :: Param.SpecialLengthParameter -> Q.Length -> EHexState m ()
  GetHexCode :: Code.CCodeType c -> Code.CharCode -> EHexState m (HSt.Code.CodeTableTarget c)
  SetHexCode :: Code.CCodeType c -> Code.CharCode -> HSt.Code.CodeTableTarget c -> HSt.Grouped.ScopeFlag -> EHexState m ()
  ResolveSymbol :: HSt.Res.ControlSymbol -> EHexState m (Maybe ResolvedToken)
  SetSymbol :: HSt.Res.ControlSymbol -> ResolvedToken -> HSt.Grouped.ScopeFlag -> EHexState m ()
  LoadFont :: HexFilePath -> TFM.FontSpecification -> EHexState m DVI.FontDefinition
  SelectFont :: DVI.FontNumber -> HSt.Grouped.ScopeFlag -> EHexState m ()
  SetFamilyMemberFont :: Font.FamilyMember -> DVI.FontNumber -> HSt.Grouped.ScopeFlag -> EHexState m ()
  CurrentFontNumber :: EHexState m DVI.FontNumber
  SetFontSpecialCharacter :: Font.FontSpecialChar -> DVI.FontNumber -> Q.HexInt -> EHexState m ()
  CurrentFontCharacter :: Code.CharCode -> EHexState m (Maybe CharacterAttrs)
  CurrentFontSpaceGlue :: EHexState m (Maybe Q.Glue)
  PopAfterAssignmentToken :: EHexState m (Maybe LT.LexToken)
  SetAfterAssignmentToken :: LT.LexToken -> EHexState m ()
  PushGroup :: Maybe Grouped.ScopedGroupType -> EHexState m ()
  PopGroup :: Grouped.ChangeGroupTrigger -> EHexState m HSt.Grouped.HexGroupType
  EnterMode :: HSt.Mode.NonMainVMode -> EHexState m ()
  PeekMode :: EHexState m HSt.Mode.ModeWithVariant
  LeaveMode :: EHexState m ()

makeEffect ''EHexState

getParIndentBox :: EHexState :> es => Eff es ListElem.HListElem
getParIndentBox = do
  boxWidth <- getParameterValue (Param.LengthQuantParam Param.ParIndent)
  pure $
    ListElem.HVListElem $
      ListElem.VListBaseElem $
        BoxElem.ElemBox $
          BoxElem.BaseBox $
            Box.Box
              { contents = BoxElem.HBoxContents (BoxElem.HBoxElemSeq Empty),
                boxWidth,
                boxHeight = mempty,
                boxDepth = mempty
              }

modifyParameterValue ::
  forall (q :: Q.QuantityType) es.
  EHexState :> es =>
  Param.QuantParam q ->
  ( Var.QuantVariableTarget q ->
    Var.QuantVariableTarget q
  ) ->
  HSt.Grouped.ScopeFlag ->
  Eff es ()
modifyParameterValue p f scopeFlag = do
  currentVal <- getParameterValue p
  setParameterValue p (f currentVal) scopeFlag

modifyRegisterValue ::
  EHexState :> es =>
  Reg.QuantRegisterLocation q ->
  ( Var.QuantVariableTarget q ->
    Var.QuantVariableTarget q
  ) ->
  HSt.Grouped.ScopeFlag ->
  Eff es ()
modifyRegisterValue loc f scopeFlag = do
  currentVal <- getRegisterValue loc
  setQuantRegisterValue loc (f currentVal) scopeFlag

advanceParameterValue ::
  forall q es.
  (EHexState :> es, Semigroup (Var.QuantVariableTarget q)) =>
  Param.QuantParam q ->
  Var.QuantVariableTarget q ->
  HSt.Grouped.ScopeFlag ->
  Eff es ()
advanceParameterValue p plusVal =
  modifyParameterValue @q p (\v -> v <> plusVal)

scaleParameterValue ::
  forall q es.
  (EHexState :> es, Q.Scalable (Var.QuantVariableTarget q)) =>
  Param.QuantParam q ->
  VDirection ->
  Q.HexInt ->
  HSt.Grouped.ScopeFlag ->
  Eff es ()
scaleParameterValue p scaleDirection arg scopeFlag =
  modifyParameterValue @q p (Q.scaleInDirection scaleDirection arg) scopeFlag

advanceRegisterValue ::
  (EHexState :> es, Semigroup (Var.QuantVariableTarget q)) =>
  Reg.QuantRegisterLocation q ->
  Var.QuantVariableTarget q ->
  HSt.Grouped.ScopeFlag ->
  Eff es ()
advanceRegisterValue loc plusVal =
  modifyRegisterValue loc (\v -> v <> plusVal)

scaleRegisterValue ::
  (EHexState :> es, Q.Scalable (Var.QuantVariableTarget q)) =>
  Reg.QuantRegisterLocation q ->
  VDirection ->
  Q.HexInt ->
  HSt.Grouped.ScopeFlag ->
  Eff es ()
scaleRegisterValue qLoc scaleDirection arg scopeFlag =
  modifyRegisterValue qLoc (Q.scaleInDirection scaleDirection arg) scopeFlag

resolveLexToken ::
  [Error ResolutionError, EHexState] :>> es =>
  LT.LexToken ->
  Eff es RT.ResolvedToken
resolveLexToken = \case
  LT.ControlSequenceLexToken cs -> do
    resolveSymbolWithError (HSt.Res.ControlSequenceSymbol cs)
  LT.CharCatLexToken (LT.LexCharCat c Code.Active) ->
    resolveSymbolWithError $ HSt.Res.ActiveCharacterSymbol c
  LT.CharCatLexToken lexCharCat ->
    pure $ RT.PrimitiveToken $ PT.CharCatPair lexCharCat
  where
    resolveSymbolWithError cSym =
      resolveSymbol cSym >>= \case
        Nothing -> throwError $ UnknownSymbolError cSym
        Just rt -> pure rt
