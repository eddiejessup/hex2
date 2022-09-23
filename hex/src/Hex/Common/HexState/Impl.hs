{-# LANGUAGE UndecidableInstances #-}

module Hex.Common.HexState.Impl where

import Data.Map.Strict qualified as Map
import Data.Text qualified as Tx
import Formatting qualified as F
import Hex.Capability.Log.Interface (HexLog (..), debugLog)
import Hex.Capability.Log.Interface qualified as Log
import Hex.Common.Codes qualified as Code
import Hex.Common.Font qualified as Font
import Hex.Common.HexEnv.Interface (EHexEnv)
import Hex.Common.HexEnv.Interface qualified as HEnv
import Hex.Common.HexState.Impl.Error qualified as Err
import Hex.Common.HexState.Impl.Font qualified as HSt.Font
import Hex.Common.HexState.Impl.Scoped.Code qualified as Sc.Code
import Hex.Common.HexState.Impl.Scoped.Font qualified as Sc.Font
import Hex.Common.HexState.Impl.Scoped.Group qualified as Sc.Group
import Hex.Common.HexState.Impl.Scoped.GroupScopes qualified as Sc.GroupScopes
import Hex.Common.HexState.Impl.Scoped.Parameter qualified as Sc.P
import Hex.Common.HexState.Impl.Scoped.Register qualified as Sc.R
import Hex.Common.HexState.Impl.Scoped.Scope (Scope)
import Hex.Common.HexState.Impl.Scoped.Symbol qualified as Sc.Sym
import Hex.Common.HexState.Impl.Type
import Hex.Common.HexState.Interface
import Hex.Common.HexState.Interface.Font qualified as HSt.Font
import Hex.Common.HexState.Interface.Grouped qualified as HSt.Grouped
import Hex.Common.HexState.Interface.Parameter qualified as HSt.Param
import Hex.Common.HexState.Interface.Register qualified as HSt.Reg
import Hex.Common.HexState.Interface.Variable qualified as HSt.Var
import Hex.Common.Quantity qualified as Q
import Hex.Common.TFM.Get qualified as TFM
import Hex.Common.TFM.Types qualified as TFM
import Hex.Stage.Build.BoxElem qualified as BoxElem
import Hexlude
import System.FilePath qualified as FilePath

runHexState :: [HexLog, State HexState, EHexEnv, Error Err.HexStateError, Error TFM.TFMError] :>> es => Eff (EHexState : es) a -> Eff es a
runHexState = interpret $ \_ -> \case
  SetScopedValue scopedValue scopeFlag ->
    let setter = case scopedValue of
          QuantRegisterValue loc val ->
            setScopedPropertyImpl (Sc.R.quantRegisterValueLens loc) (Just val)
          ParameterValue param val ->
            setScopedPropertyImpl (Sc.P.parameterValueLens param) (Just val)
          HexCodeValue codeType idxCode tgt ->
            setScopedPropertyImpl (Sc.Code.hexCodeLens codeType idxCode) (Just tgt)
          SymbolValue symbol rt ->
            setScopedPropertyImpl (Sc.Sym.symbolLens symbol) (Just rt)
          FontValue fontNumber ->
            setScopedPropertyImpl Sc.Font.currentFontNrLens (Just fontNumber)
          FamilyMemberFontValue familyMember fontNumber ->
            setScopedPropertyImpl (Sc.Font.familyMemberFontLens familyMember) (Just fontNumber)
          BoxRegisterValue loc mayElem ->
            setScopedPropertyImpl (Sc.R.boxRegisterValueLens loc) mayElem
     in setter scopeFlag
  GetParameterValue p ->
    getParameterValueImpl p
  GetRegisterValue r -> getGroupScopesProperty (Sc.R.localQuantRegisterValue r)
  FetchBoxRegisterValue fetchMode rLoc -> fetchBoxRegisterValueImpl fetchMode rLoc
  GetSpecialIntParameter p -> getSpecialIntParameterImpl p
  GetSpecialLengthParameter p -> getSpecialLengthParameterImpl p
  SetSpecialIntParameter p v -> setSpecialIntParameterImpl p v
  SetSpecialLengthParameter p v -> setSpecialLengthParameterImpl p v
  GetHexCode t p -> getGroupScopesProperty (Sc.Code.localHexCode t p)
  ResolveSymbol p -> getGroupScopesProperty (Sc.Sym.localResolvedToken p)
  LoadFont fontPath spec -> loadFontImpl fontPath spec
  GetAllFontDefinitions -> getAllFontDefinitionsImpl
  CurrentFontNumber -> currentFontNumberImpl
  SpaceGlue fNr -> do
    sf <- getSpecialIntParameterImpl HSt.Param.SpaceFactor
    spaceGlueFromSpaceFactor fNr sf
  ControlSpaceGlue fNr ->
    spaceGlueFromSpaceFactor fNr (Q.thousandInt)
  FontCharacter fNr chrCode -> fontCharacterImpl fNr chrCode
  SetFontSpecialCharacter fontSpecialChar fontNumber value -> setFontSpecialCharacterImpl fontSpecialChar fontNumber value
  GetFontSpecialCharacter fontSpecialChar fontNumber -> getFontSpecialCharacterImpl fontSpecialChar fontNumber
  SetAfterAssignmentToken t -> assign @HexState #afterAssignmentToken (Just t)
  PopAfterAssignmentToken -> do
    v <- use @HexState #afterAssignmentToken
    assign @HexState #afterAssignmentToken Nothing
    pure v
  PushGroup mayScopedGroupType -> do
    debugLog "pushGroup"
    modifying @HexState (#groupScopes) (Sc.GroupScopes.pushGroup mayScopedGroupType)
  PopGroup exitTrigger -> popGroupImpl exitTrigger
  EnterMode mode -> do
    modifying @HexState #modeStack (enterModeImpl mode)
  LeaveMode -> do
    use @HexState (#modeStack % to leaveModeImpl) >>= \case
      Nothing -> throwError Err.TriedToLeaveMainVMode
      Just newModeStack -> assign @HexState #modeStack newModeStack
  PeekMode ->
    use @HexState (#modeStack % to peekModeImpl)
  SetHyphenationPatterns patterns ->
    assign @HexState #hyphenationPatterns patterns
  SetHyphenationExceptions hyphExceptions -> do
    modifying @HexState #hyphenationExceptions (Map.union hyphExceptions)

getParameterValueImpl ::
  State HexState :> es =>
  HSt.Param.QuantParam q ->
  Eff es (HSt.Var.QuantVariableTarget q)
getParameterValueImpl p = getGroupScopesProperty (Sc.P.localParameterValue p)

getSpecialIntParameterImpl :: (State HexState :> es) => HSt.Param.SpecialIntParameter -> Eff es Q.HexInt
getSpecialIntParameterImpl p = use $ stateSpecialIntParamLens p

getSpecialLengthParameterImpl :: (State HexState :> es) => HSt.Param.SpecialLengthParameter -> Eff es Q.Length
getSpecialLengthParameterImpl p = use $ stateSpecialLengthParamLens p

setSpecialIntParameterImpl ::
  (State HexState :> es, HexLog :> es) =>
  HSt.Param.SpecialIntParameter ->
  Q.HexInt ->
  Eff es ()
setSpecialIntParameterImpl p v = do
  assign (stateSpecialIntParamLens p) v
  Log.debugLog $ F.sformat (HSt.Param.fmtSpecialIntParameter |%| " := " |%| Q.fmtHexInt) p v

setSpecialLengthParameterImpl ::
  (State HexState :> es, HexLog :> es) =>
  HSt.Param.SpecialLengthParameter ->
  Q.Length ->
  Eff es ()
setSpecialLengthParameterImpl p v = do
  assign (stateSpecialLengthParamLens p) v
  Log.debugLog $ F.sformat (HSt.Param.fmtSpecialLengthParameter |%| " := " |%| Q.fmtLengthWithUnit) p v

setScopedPropertyImpl :: State HexState :> es => (Lens' Scope (Maybe a)) -> Maybe a -> HSt.Grouped.ScopeFlag -> Eff es ()
setScopedPropertyImpl scopeValueLens a scopeFlag =
  modifying @HexState
    #groupScopes
    (Sc.GroupScopes.setScopedProperty scopeValueLens a scopeFlag)

fontCharacterImpl ::
  State HexState :> es =>
  Font.FontNumber ->
  Code.CharCode ->
  Eff es (Maybe HSt.Font.CharacterAttrs)
fontCharacterImpl fNr chrCode = do
  fInfo <- fontInfoImpl fNr
  pure $ HSt.Font.characterAttrs fInfo chrCode

setFontSpecialCharacterImpl ::
  (State HexState :> es) =>
  HSt.Font.FontSpecialChar ->
  Font.FontNumber ->
  Q.HexInt ->
  Eff es ()
setFontSpecialCharacterImpl fontSpecialChar fNr value = do
  let fontSpecialCharLens = case fontSpecialChar of
        HSt.Font.HyphenChar -> #hyphenChar
        HSt.Font.SkewChar -> #skewChar
  -- (Assert that the font number is valid, just to check for correctness.)
  _ <- fontInfoImpl fNr
  assign @HexState (#fontInfos % at' fNr %? fontSpecialCharLens) value

getFontSpecialCharacterImpl ::
  (State HexState :> es) =>
  HSt.Font.FontSpecialChar ->
  Font.FontNumber ->
  Eff es Q.HexInt
getFontSpecialCharacterImpl fontSpecialChar fontNumber = do
  use @HexState $
    stateFontInfoLens fontNumber % case fontSpecialChar of
      HSt.Font.HyphenChar -> #hyphenChar
      HSt.Font.SkewChar -> #skewChar

readFontInfo ::
  ([Error TFM.TFMError, State HexState] :>> es) =>
  HexFilePath ->
  ByteString ->
  TFM.FontSpecification ->
  Eff es HSt.Font.FontInfo
readFontInfo fontPath fontBytes spec = do
  fontMetrics <- TFM.parseTFMBytes fontBytes
  hyphenChar <- getParameterValueImpl (HSt.Param.IntQuantParam HSt.Param.DefaultHyphenChar)
  skewChar <- getParameterValueImpl (HSt.Param.IntQuantParam HSt.Param.DefaultSkewChar)
  let designScale = TFM.fontSpecToDesignScale fontMetrics.designFontSize spec
  pure HSt.Font.FontInfo {fontMetrics, designScale, hyphenChar, skewChar, fontPath}

loadFontImpl ::
  [State HexState, EHexEnv, Error Err.HexStateError, Error TFM.TFMError] :>> es =>
  HexFilePath ->
  TFM.FontSpecification ->
  Eff es Font.FontNumber
loadFontImpl fontPath spec = do
  fontBytes <-
    HEnv.findAndReadFile
      (HEnv.WithImplicitExtension "tfm")
      fontPath
      >>= note (Err.FontNotFound fontPath)

  fontInfo <- readFontInfo fontPath fontBytes spec

  mayLastKey <- use @HexState $ #fontInfos % to Map.lookupMax
  let fontNr = case mayLastKey of
        Nothing -> Font.FontNumber $ Q.HexInt 0
        Just (i, _) -> succ i
  assign @HexState (#fontInfos % at' fontNr) (Just fontInfo)
  pure fontNr

getAllFontDefinitionsImpl ::
  '[State HexState] :>> es =>
  Eff es [HSt.Font.FontDefinition]
getAllFontDefinitionsImpl =
  use @HexState #fontInfos <&> Map.elems . Map.mapWithKey toFontDefinition
  where
    toFontDefinition fontNr fontInfo =
      HSt.Font.FontDefinition
        { fontDefChecksum = fontInfo.fontMetrics.checksum,
          fontDefDesignSize = fontInfo.fontMetrics.designFontSize,
          fontDefDesignScale = fontInfo.designScale,
          fontPath = fontInfo.fontPath,
          fontName = Tx.pack (FilePath.takeBaseName fontInfo.fontPath.unHexFilePath),
          fontNr
        }

fetchBoxRegisterValueImpl ::
  State HexState :> es =>
  HSt.Reg.BoxFetchMode ->
  HSt.Reg.RegisterLocation ->
  Eff es (Maybe BoxElem.BaseBox)
fetchBoxRegisterValueImpl fetchMode loc = do
  mayV <- getGroupScopesProperty (Sc.R.localBoxRegisterValue loc)
  case fetchMode of
    HSt.Reg.Pop -> setScopedPropertyImpl (Sc.R.boxRegisterValueLens loc) Nothing HSt.Grouped.LocalScope
    HSt.Reg.Lookup -> pure ()
  pure mayV

popGroupImpl ::
  [State HexState, Error Err.HexStateError] :>> es =>
  HSt.Grouped.ChangeGroupTrigger ->
  Eff es HSt.Grouped.HexGroupType
popGroupImpl exitTrigger = do
  use @HexState (#groupScopes % to Sc.GroupScopes.popGroup) >>= \case
    Nothing ->
      throwError Err.PoppedEmptyGroups
    Just (poppedGroup, newGroupScopes) ->
      case poppedGroup of
        Sc.Group.ScopeGroup (Sc.Group.GroupScope _scope scopeGroupType) -> do
          assign @HexState (#groupScopes) newGroupScopes
          poppedGroupType <- case scopeGroupType of
            HSt.Grouped.LocalStructureScopeGroup enterTrigger
              | exitTrigger == enterTrigger ->
                  pure HSt.Grouped.LocalStructureGroupType
              | otherwise ->
                  throwError Err.UnmatchedExitGroupTrigger
            HSt.Grouped.ExplicitBoxScopeGroup ->
              case exitTrigger of
                HSt.Grouped.ChangeGroupCharTrigger ->
                  pure HSt.Grouped.ExplicitBoxGroupType
                HSt.Grouped.ChangeGroupCSTrigger ->
                  throwError Err.UnmatchedExitGroupTrigger
          pure poppedGroupType
        Sc.Group.NonScopeGroup ->
          notImplemented $ "popGroup: NonScopeGroup"

spaceGlueFromSpaceFactor ::
  State HexState :> es =>
  Font.FontNumber ->
  Q.HexInt ->
  Eff es Q.Glue
spaceGlueFromSpaceFactor fNr sf = do
  -- TODO: Note that, if \spaceskip and \xspaceskip are defined in terms of em, they change with the font.
  let sfAbove2k = sf >= Q.HexInt 2000
  xSpaceSkip <- getParameterValueImpl (HSt.Param.GlueQuantParam HSt.Param.XSpaceSkip)
  if (xSpaceSkip /= Q.zeroGlue && sfAbove2k)
    then pure xSpaceSkip
    else do
      -- "If \spaceskip is non-zero, it is taken instead of the normal interword
      -- space"
      spaceSkip <- getParameterValueImpl (HSt.Param.GlueQuantParam HSt.Param.SpaceSkip)
      if spaceSkip /= Q.zeroGlue
        then pure $ scaleSpaceFlex spaceSkip
        else do
          fontInfo <- fontInfoImpl fNr
          let baseSpacing = HSt.Font.fontLengthParamLength fontInfo (.spacing)
              baseStretch = Q.FinitePureFlex $ HSt.Font.fontLengthParamLength fontInfo (.spaceStretch)
              baseShrink = Q.FinitePureFlex $ HSt.Font.fontLengthParamLength fontInfo (.spaceShrink)
              extraSpace = HSt.Font.fontLengthParamLength fontInfo (.extraSpace)

              adjustedSpacing =
                if sfAbove2k
                  then baseSpacing <> extraSpace
                  else baseSpacing

              baseGlue = Q.Glue {gDimen = adjustedSpacing, gStretch = baseStretch, gShrink = baseShrink}
          pure $ scaleSpaceFlex baseGlue
  where
    scaleSpaceFlex g =
      let sfRatio = Q.inThousands sf
       in g
            { Q.gStretch = Q.scalePureFlexByRational sfRatio g.gStretch,
              Q.gShrink = Q.scalePureFlexByRational (recip sfRatio) g.gShrink
            }
