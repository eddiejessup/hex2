{-# LANGUAGE UndecidableInstances #-}

module Hex.Common.HexState.Impl where

import Data.Map.Strict qualified as Map
import Data.Text qualified as Tx
import Hex.Capability.Log.Interface (HexLog (..), debugLog)
import Hex.Common.Codes qualified as Code
import Hex.Common.HexEnv.Interface (EHexEnv)
import Hex.Common.HexEnv.Interface qualified as Env
import Hex.Common.HexState.Impl.Error qualified as Err
import Hex.Common.HexState.Impl.Font qualified as HSt.Font
import Hex.Common.HexState.Impl.Scoped.Code qualified as Sc.Code
import Hex.Common.HexState.Impl.Scoped.Font qualified as Sc.Font
import Hex.Common.HexState.Impl.Scoped.Group qualified as Sc.Group
import Hex.Common.HexState.Impl.Scoped.GroupScopes (GroupScopes)
import Hex.Common.HexState.Impl.Scoped.GroupScopes qualified as Sc.GroupScopes
import Hex.Common.HexState.Impl.Scoped.Parameter qualified as Sc.P
import Hex.Common.HexState.Impl.Scoped.Register qualified as Sc.R
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
import Hex.Stage.Render.Interface.DocInstruction qualified as DVI
import Hexlude
import System.FilePath qualified as FilePath

runHexState :: [HexLog, State HexState, EHexEnv, Error Err.HexStateError, Error TFM.TFMError] :>> es => Eff (EHexState : es) a -> Eff es a
runHexState = interpret $ \_ -> \case
  GetParameterValue p -> getParameterValueImpl p
  SetParameterValue param value scopeFlag ->
    modifyGroupScopes $ Sc.P.setParameterValue param value scopeFlag
  GetRegisterValue r -> getGroupScopesProperty (Sc.R.localQuantRegisterValue r)
  SetQuantRegisterValue param value scopeFlag ->
    modifyGroupScopes $ Sc.R.setQuantRegisterValue param value scopeFlag
  FetchBoxRegisterValue fetchMode rLoc -> fetchBoxRegisterValueImpl fetchMode rLoc
  SetBoxRegisterValue rLoc tgt scope -> setBoxRegisterValueImpl rLoc tgt scope
  GetSpecialIntParameter p -> use $ stateSpecialIntParamLens p
  GetSpecialLengthParameter p -> use $ stateSpecialLengthParamLens p
  SetSpecialIntParameter p v -> assign (stateSpecialIntParamLens p) v
  SetSpecialLengthParameter p v -> assign (stateSpecialLengthParamLens p) v
  GetHexCode t p -> getGroupScopesProperty (Sc.Code.localHexCode t p)
  SetHexCode t idxCode code scopeFlag -> modifyGroupScopes $ Sc.Code.setHexCode t idxCode code scopeFlag
  ResolveSymbol p -> getGroupScopesProperty (Sc.Sym.localResolvedToken p)
  SetSymbol symbol target scopeFlag ->
    modifyGroupScopes $ Sc.Sym.setSymbol symbol target scopeFlag
  LoadFont fontPath spec -> loadFontImpl fontPath spec
  CurrentFontNumber -> currentFontNumberImpl
  CurrentFontSpaceGlue ->
    HSt.Font.fontSpaceGlue <$> currentFontInfo
  CurrentFontCharacter chrCode -> currentFontCharacterImpl chrCode
  SelectFont fontNumber scopeFlag ->
    modifyGroupScopes $ Sc.Font.setCurrentFontNr fontNumber scopeFlag
  SetFamilyMemberFont familyMember fontNumber scopeFlag ->
    modifyGroupScopes $ Sc.Font.setFamilyMemberFont familyMember fontNumber scopeFlag
  SetFontSpecialCharacter fontSpecialChar fontNumber value -> do
    let fontSpecialCharLens = case fontSpecialChar of
          HSt.Font.HyphenChar -> #hyphenChar
          HSt.Font.SkewChar -> #skewChar
    use @HexState (#fontInfos % at' fontNumber) >>= \case
      Nothing -> throwError Err.MissingFontNumber
      Just _ -> pure ()
    assign @HexState (#fontInfos % at' fontNumber %? fontSpecialCharLens) value
  SetAfterAssignmentToken t -> assign @HexState #afterAssignmentToken (Just t)
  PopAfterAssignmentToken -> do
    v <- use @HexState #afterAssignmentToken
    assign @HexState #afterAssignmentToken Nothing
    pure v
  PushGroup mayScopedGroupType -> do
    debugLog "pushGroup"
    modifyGroupScopes $ Sc.GroupScopes.pushGroup mayScopedGroupType
  PopGroup exitTrigger -> popGroupImpl exitTrigger
  EnterMode mode -> do
    modifying @HexState #modeStack (enterModeImpl mode)
  LeaveMode -> do
    use @HexState (#modeStack % to leaveModeImpl) >>= \case
      Nothing -> throwError Err.TriedToLeaveMainVMode
      Just newModeStack -> assign @HexState #modeStack newModeStack
  PeekMode ->
    use @HexState (#modeStack % to peekModeImpl)

getParameterValueImpl ::
  State HexState :> es =>
  HSt.Param.QuantParam q ->
  Eff es (HSt.Var.QuantVariableTarget q)
getParameterValueImpl p = getGroupScopesProperty (Sc.P.localParameterValue p)

modifyGroupScopes :: State HexState :> es => (GroupScopes -> GroupScopes) -> Eff es ()
modifyGroupScopes = modifying @HexState (#groupScopes)

currentFontInfo ::
  [State HexState, Error Err.HexStateError] :>> es =>
  Eff es HSt.Font.FontInfo
currentFontInfo =
  currentFontInfoImpl
    >>= note Err.MissingFontNumber

currentFontCharacterImpl ::
  (State HexState :> es, Error Err.HexStateError :> es) =>
  Code.CharCode ->
  Eff es (Maybe HSt.Font.CharacterAttrs)
currentFontCharacterImpl chrCode = do
  fInfo <- currentFontInfo
  pure $ HSt.Font.characterAttrs fInfo chrCode

readFontInfo ::
  ([Error TFM.TFMError, State HexState] :>> es) =>
  ByteString ->
  TFM.FontSpecification ->
  Eff es HSt.Font.FontInfo
readFontInfo fontBytes spec = do
  fontMetrics <- TFM.parseTFMBytes fontBytes
  hyphenChar <- getParameterValueImpl (HSt.Param.IntQuantParam HSt.Param.DefaultHyphenChar)
  skewChar <- getParameterValueImpl (HSt.Param.IntQuantParam HSt.Param.DefaultSkewChar)
  let designScale = TFM.fontSpecToDesignScale fontMetrics.designFontSize spec
  pure HSt.Font.FontInfo {fontMetrics, designScale, hyphenChar, skewChar}

loadFontImpl ::
  [State HexState, Error Err.HexStateError, Error TFM.TFMError, EHexEnv] :>> es =>
  HexFilePath ->
  TFM.FontSpecification ->
  Eff es DVI.FontDefinition
loadFontImpl fontPath spec = do
  fontBytes <-
    Env.findAndReadFile
      (Env.WithImplicitExtension "tfm")
      (fontPath ^. typed @FilePath)
      >>= note (Err.FontNotFound fontPath)

  fontInfo <- readFontInfo fontBytes spec

  let designScale = TFM.fontSpecToDesignScale fontInfo.fontMetrics.designFontSize spec

  mayLastKey <- use @HexState $ #fontInfos % to Map.lookupMax
  let fontNr = case mayLastKey of
        Nothing -> DVI.FontNumber $ Q.HexInt 0
        Just (i, _) -> succ i
  assign @HexState (#fontInfos % at' fontNr) (Just fontInfo)

  let fontName = Tx.pack $ FilePath.takeBaseName (fontPath ^. typed @FilePath)

      fontDef =
        DVI.FontDefinition
          { fontDefChecksum = fontInfo.fontMetrics.checksum,
            fontDefDesignSize = fontInfo.fontMetrics.designFontSize,
            fontDefDesignScale = designScale,
            fontPath,
            fontName,
            fontNr
          }

  pure fontDef

fetchBoxRegisterValueImpl ::
  State HexState :> es =>
  HSt.Reg.BoxFetchMode ->
  HSt.Reg.RegisterLocation ->
  Eff es (Maybe BoxElem.BaseBox)
fetchBoxRegisterValueImpl fetchMode loc = do
  mayV <- getGroupScopesProperty (Sc.R.localBoxRegisterValue loc)
  case fetchMode of
    HSt.Reg.Pop -> modifyGroupScopes $ Sc.R.unsetBoxRegisterValue loc HSt.Grouped.LocalScope
    HSt.Reg.Lookup -> pure ()
  pure mayV

setBoxRegisterValueImpl ::
  State HexState :> es =>
  HSt.Reg.RegisterLocation ->
  Maybe BoxElem.BaseBox ->
  HSt.Grouped.ScopeFlag ->
  Eff es ()
setBoxRegisterValueImpl loc mayV scope =
  modifyGroupScopes $ case mayV of
    Nothing -> Sc.R.unsetBoxRegisterValue loc scope
    Just v -> Sc.R.setBoxRegisterValue loc v scope

popGroupImpl ::
  [State HexState, Error Err.HexStateError] :>> es =>
  HSt.Grouped.ChangeGroupTrigger ->
  Eff es (HSt.Grouped.HexGroupType, Maybe DVI.FontNumber)
popGroupImpl exitTrigger = do
  fontNrPrePop <- currentFontNumberImpl
  use @HexState (#groupScopes % to Sc.GroupScopes.popGroup) >>= \case
    Nothing ->
      throwError Err.PoppedEmptyGroups
    Just (poppedGroup, newGroupScopes) ->
      case poppedGroup of
        Sc.Group.ScopeGroup (Sc.Group.GroupScope _scope scopeGroupType) -> do
          assign @HexState (#groupScopes) newGroupScopes
          fontNrPostPop <- currentFontNumberImpl
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
          let fontNrToSet =
                if fontNrPrePop /= fontNrPostPop
                  then Just fontNrPostPop
                  else Nothing
          pure (poppedGroupType, fontNrToSet)
        Sc.Group.NonScopeGroup ->
          notImplemented $ "popGroup: NonScopeGroup"
