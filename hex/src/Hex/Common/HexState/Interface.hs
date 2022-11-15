{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE TemplateHaskell #-}

module Hex.Common.HexState.Interface where

import Formatting qualified as F
import Hex.Common.Box qualified as Box
import Hex.Common.Codes qualified as Code
import Hex.Common.Font qualified as Font
import Hex.Common.HexState.Interface.Code qualified as HSt.Code
import Hex.Common.HexState.Interface.Font qualified as Font
import Hex.Common.HexState.Interface.Font qualified as HSt.Font
import Hex.Common.HexState.Interface.Grouped qualified as Grouped
import Hex.Common.HexState.Interface.Grouped qualified as HSt.Grouped
import Hex.Common.HexState.Interface.Hyphen qualified as HSt.Hyph
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
import Hexlude

data ResolutionError = UnknownSymbolError HSt.Res.ControlSymbol
  deriving stock (Show, Generic)

fmtResolutionError :: Fmt ResolutionError
fmtResolutionError = F.later $ \case
  UnknownSymbolError cSym ->
    "Got unknown control-symbol: " <> F.bformat HSt.Res.fmtControlSymbol cSym

data ScopedValue
  = forall (q :: Q.QuantityType). QuantRegisterValue (Reg.QuantRegisterLocation q) (Var.QuantVariableTarget q)
  | forall (q :: Q.QuantityType). ParameterValue (Param.QuantParam q) (Var.QuantVariableTarget q)
  | forall (c :: Code.CodeType). HexCodeValue (Code.CCodeType c) Code.CharCode (HSt.Code.CodeTableTarget c)
  | SymbolValue HSt.Res.ControlSymbol ResolvedToken
  | FontValue Font.FontNumber
  | FamilyMemberFontValue Font.FamilyMember Font.FontNumber
  | BoxRegisterValue Reg.RegisterLocation (Maybe (Box.Boxed (BoxElem.AxBoxElems)))

data EHexState :: Effect where
  -- Get scoped values.
  GetRegisterValue :: Reg.QuantRegisterLocation q -> EHexState m (Var.QuantVariableTarget q)
  GetParameterValue :: Param.QuantParam q -> EHexState m (Var.QuantVariableTarget q)
  GetHexCode :: Code.CCodeType c -> Code.CharCode -> EHexState m (HSt.Code.CodeTableTarget c)
  ResolveSymbol :: HSt.Res.ControlSymbol -> EHexState m (Maybe ResolvedToken)
  CurrentFontNumber :: EHexState m Font.FontNumber
  FetchBoxRegisterValue :: Reg.BoxFetchMode -> Reg.RegisterLocation -> EHexState m (Maybe (Box.Boxed (BoxElem.AxBoxElems)))
  -- End of scoped-value getters.
  -- Scoped-value setter.
  SetScopedValue :: ScopedValue -> HSt.Grouped.ScopeFlag -> EHexState m ()
  -- Non-scoped value getter and setters.
  GetSpecialIntParameter :: Param.SpecialIntParameter -> EHexState m Q.HexInt
  GetSpecialLengthParameter :: Param.SpecialLengthParameter -> EHexState m Q.Length
  SetSpecialIntParameter :: Param.SpecialIntParameter -> Q.HexInt -> EHexState m ()
  SetSpecialLengthParameter :: Param.SpecialLengthParameter -> Q.Length -> EHexState m ()
  -- Fonts.
  LoadFont :: HexFilePath -> TFM.FontSpecification -> EHexState m Font.FontNumber
  GetAllFontDefinitions :: EHexState m [HSt.Font.FontDefinition]
  SetFontSpecialCharacter :: Font.FontSpecialChar -> Font.FontNumber -> Q.HexInt -> EHexState m ()
  GetFontSpecialCharacter :: Font.FontSpecialChar -> Font.FontNumber -> EHexState m Q.HexInt
  GetFontLengthParameter :: Font.FontNumber -> TFM.FontLengthParam -> EHexState m Q.Length
  FontCharacter :: Font.FontNumber -> Code.CharCode -> EHexState m (Maybe HSt.Font.CharacterAttrs)
  SpaceGlue :: Font.FontNumber -> EHexState m Q.Glue
  ControlSpaceGlue :: Font.FontNumber -> EHexState m Q.Glue
  -- Scoping.
  PopAfterAssignmentToken :: EHexState m (Maybe LT.LexToken)
  SetAfterAssignmentToken :: LT.LexToken -> EHexState m ()
  PushGroup :: Maybe Grouped.ScopedGroupType -> EHexState m ()
  PopGroup :: Grouped.ChangeGroupTrigger -> EHexState m HSt.Grouped.HexGroupType
  -- Modes.
  EnterMode :: HSt.Mode.NonMainVMode -> EHexState m ()
  PeekMode :: EHexState m HSt.Mode.ModeWithVariant
  LeaveMode :: EHexState m ()
  -- Hyphenation.
  SetHyphenationPatterns :: [HSt.Hyph.HyphenationPattern] -> EHexState m ()
  SetHyphenationExceptions :: Map HSt.Hyph.WordCodes HSt.Hyph.WordHyphenationPoints -> EHexState m ()

makeEffect ''EHexState

getParIndentBoxDims :: EHexState :> es => Eff es (Box.BoxDims Q.Length)
getParIndentBoxDims = do
  boxWidth <- getParameterValue (Param.LengthQuantParam Param.ParIndent)
  pure Box.BoxDims {boxWidth, boxHeight = mempty, boxDepth = mempty}

emptyHBoxAsHListElem :: Box.BoxDims Q.Length -> ListElem.HListElem
emptyHBoxAsHListElem boxedDims =
  let emptyHElems =
        BoxElem.AxBoxOrRuleContentsAx
          Box.Offsettable {offset = Nothing, offsetContents = BoxElem.AxBoxElemsH Empty}
   in ListElem.HVListElem $
        ListElem.VListBaseElem $
          BoxElem.AxOrRuleBoxBaseElem $
            Box.Boxed {boxedDims, boxedContents = emptyHElems}

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
  setScopedValue (ParameterValue p (f currentVal)) scopeFlag

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
  setScopedValue (QuantRegisterValue loc (f currentVal)) scopeFlag

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
  (Error ResolutionError :> es, EHexState :> es) =>
  LT.LexToken ->
  Eff es RT.ResolvedToken
resolveLexToken lt =
  case lexTokenAsSymbol lt of
    Left lexCharCat ->
      pure $ RT.PrimitiveToken $ PT.CharCatPair lexCharCat
    Right cSym ->
      resolveSymbol cSym >>= \case
        Nothing -> throwError $ UnknownSymbolError cSym
        Just rt -> pure rt

lexTokenAsSymbol ::
  LT.LexToken ->
  Either LT.LexCharCat HSt.Res.ControlSymbol
lexTokenAsSymbol = \case
  LT.ControlSequenceLexToken cs -> do
    Right $ HSt.Res.ControlSequenceSymbol cs
  LT.CharCatLexToken (LT.LexCharCat c Code.Active) ->
    Right $ HSt.Res.ActiveCharacterSymbol c
  LT.CharCatLexToken lexCharCat ->
    Left lexCharCat

getHyphenCharCode ::
  (EHexState :> es) =>
  Font.FontNumber ->
  Eff es (Maybe Code.CharCode)
getHyphenCharCode fNr = do
  hyphenCodeInt <- getFontSpecialCharacter HSt.Font.HyphenChar fNr
  pure $ Code.fromHexInt @Code.CharCode hyphenCodeInt

codeAsDiscretionaryItem ::
  (EHexState :> es) =>
  Font.FontNumber ->
  Code.CharCode ->
  Eff es (Maybe ListElem.DiscretionaryItem)
codeAsDiscretionaryItem fNr hyphenCharCode =
  charAsBox fNr hyphenCharCode
    <&> fmap ListElem.discretionaryHyphenItem

fontDiscretionaryHyphenItem ::
  (EHexState :> es) =>
  Font.FontNumber ->
  Eff es (Maybe ListElem.DiscretionaryItem)
fontDiscretionaryHyphenItem fNr = do
  getHyphenCharCode fNr >>= \case
    Nothing ->
      pure Nothing
    Just hyphenCharCode ->
      codeAsDiscretionaryItem fNr hyphenCharCode

charAsBox ::
  ( EHexState :> es
  ) =>
  Font.FontNumber ->
  Code.CharCode ->
  Eff es (Maybe (Box.Boxed BoxElem.CharBoxContents))
charAsBox fNr char = do
  fontCharacter fNr char >>= \case
    Nothing -> pure Nothing
    Just HSt.Font.CharacterAttrs {dims} ->
      pure $
        Just $
          Box.Boxed
            { boxedContents =
                BoxElem.CharBoxContents
                  { charBoxCharCode = char,
                    charBoxFont = fNr
                  },
              boxedDims = dims
            }
