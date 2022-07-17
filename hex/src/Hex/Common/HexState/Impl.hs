{-# LANGUAGE UndecidableInstances #-}

module Hex.Common.HexState.Impl where

import Data.Map.Strict qualified as Map
import Data.Text qualified as Tx
import Formatting qualified as F
import Hex.Capability.Log.Interface (MonadHexLog (..), debugLog)
import Hex.Common.Codes qualified as Code
import Hex.Common.DVI.DocInstruction qualified as DVI
import Hex.Common.HexEnv.Interface qualified as Env
import Hex.Common.HexEnv.Interface qualified as HEnv
import Hex.Common.HexState.Impl.Font qualified as Sc.Font
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
import Hex.Common.HexState.Interface.Code qualified as HSt.Code
import Hex.Common.HexState.Interface.Font qualified as HSt.Font
import Hex.Common.HexState.Interface.Grouped qualified as HSt.Grouped
import Hex.Common.HexState.Interface.Parameter qualified as HSt.Param
import Hex.Common.HexState.Interface.Register qualified as HSt.Reg
import Hex.Common.HexState.Interface.Resolve (ControlSymbol)
import Hex.Common.HexState.Interface.Variable qualified as HSt.Var
import Hex.Common.Quantity qualified as Q
import Hex.Common.TFM.Get qualified as TFM
import Hex.Common.TFM.Types qualified as TFM
import Hex.Common.Token.Lexed qualified as LT
import Hex.Common.Token.Resolved (ResolvedToken)
import Hex.Stage.Build.BoxElem qualified as BoxElem
import Hexlude
import System.FilePath qualified as FilePath

data HexStateError
  = FontNotFound HexFilePath
  | MissingFontNumber
  | BadPath Text
  | CharacterCodeNotFound
  | PoppedEmptyGroups
  | UnmatchedExitGroupTrigger
  deriving stock (Show, Generic)

fmtHexStateError :: Fmt HexStateError
fmtHexStateError = F.shown

newtype HexStateT m a = HexStateT {unHexStateT :: m a}
  deriving newtype
    ( Functor,
      Applicative,
      Monad,
      MonadIO,
      MonadState st,
      MonadReader r,
      MonadError e,
      MonadHexLog,
      HEnv.MonadHexEnv
    )

instance
  ( Monad m,
    MonadHexLog (HexStateT m),
    MonadIO (HexStateT m),
    MonadState st (HexStateT m),
    HasType HexState st,
    Env.MonadHexEnv (HexStateT m),
    MonadError e (HexStateT m),
    AsType HexStateError e,
    AsType TFM.TFMError e
  ) =>
  MonadHexState (HexStateT m)
  where
  getParameterValue :: HSt.Param.QuantParam q -> HexStateT m (HSt.Var.QuantVariableTarget q)
  getParameterValue p = getGroupScopesProperty (Sc.P.localParameterValue p)

  setParameterValue :: HSt.Param.QuantParam q -> HSt.Var.QuantVariableTarget q -> HSt.Grouped.ScopeFlag -> HexStateT m ()
  setParameterValue param value scopeFlag =
    modifyGroupScopes $ Sc.P.setParameterValue param value scopeFlag

  getRegisterValue :: HSt.Reg.QuantRegisterLocation q -> HexStateT m (HSt.Var.QuantVariableTarget q)
  getRegisterValue r = getGroupScopesProperty (Sc.R.localQuantRegisterValue r)

  setQuantRegisterValue :: HSt.Reg.QuantRegisterLocation q -> (HSt.Var.QuantVariableTarget q) -> HSt.Grouped.ScopeFlag -> HexStateT m ()
  setQuantRegisterValue param value scopeFlag =
    modifyGroupScopes $ Sc.R.setQuantRegisterValue param value scopeFlag

  fetchBoxRegisterValue :: HSt.Reg.BoxFetchMode -> HSt.Reg.RegisterLocation -> HexStateT m (Maybe BoxElem.BaseBox)
  fetchBoxRegisterValue = fetchBoxRegisterValueImpl

  setBoxRegisterValue :: HSt.Reg.RegisterLocation -> Maybe BoxElem.BaseBox -> HSt.Grouped.ScopeFlag -> HexStateT m ()
  setBoxRegisterValue = setBoxRegisterValueImpl

  getSpecialIntParameter :: HSt.Param.SpecialIntParameter -> (HexStateT m) Q.HexInt
  getSpecialIntParameter p = use $ typed @HexState % stateSpecialIntParamLens p

  getSpecialLengthParameter :: HSt.Param.SpecialLengthParameter -> (HexStateT m) Q.Length
  getSpecialLengthParameter p = use $ typed @HexState % stateSpecialLengthParamLens p

  setSpecialIntParameter :: HSt.Param.SpecialIntParameter -> Q.HexInt -> (HexStateT m) ()
  setSpecialIntParameter p v = assign' (typed @HexState % stateSpecialIntParamLens p) v

  setSpecialLengthParameter :: HSt.Param.SpecialLengthParameter -> Q.Length -> (HexStateT m) ()
  setSpecialLengthParameter p v = assign' (typed @HexState % stateSpecialLengthParamLens p) v

  getHexCode :: Code.CCodeType c -> Code.CharCode -> (HexStateT m) (HSt.Code.CodeTableTarget c)
  getHexCode t p = getGroupScopesProperty (Sc.Code.localHexCode t p)

  setHexCode :: Code.CCodeType c -> Code.CharCode -> HSt.Code.CodeTableTarget c -> HSt.Grouped.ScopeFlag -> HexStateT m ()
  setHexCode t idxCode code scopeFlag = modifyGroupScopes $ Sc.Code.setHexCode t idxCode code scopeFlag

  resolveSymbol :: ControlSymbol -> (HexStateT m) (Maybe ResolvedToken)
  resolveSymbol p = getGroupScopesProperty (Sc.Sym.localResolvedToken p)

  setSymbol :: ControlSymbol -> ResolvedToken -> HSt.Grouped.ScopeFlag -> HexStateT m ()
  setSymbol symbol target scopeFlag =
    modifyGroupScopes $ Sc.Sym.setSymbol symbol target scopeFlag

  loadFont ::
    HexFilePath ->
    TFM.FontSpecification ->
    HexStateT m DVI.FontDefinition
  loadFont fontPath spec = do
    fontBytes <-
      Env.findAndReadFile
        (Env.WithImplicitExtension "tfm")
        (fontPath ^. typed @FilePath)
        >>= note (injectTyped (FontNotFound fontPath))

    fontInfo <- readFontInfo fontBytes

    let designScale = TFM.fontSpecToDesignScale fontInfo.fontMetrics.designFontSize spec

    mayLastKey <- use $ typed @HexState % #fontInfos % to Map.lookupMax
    let fontNr = case mayLastKey of
          Nothing -> DVI.FontNumber $ Q.HexInt 0
          Just (i, _) -> succ i
    assign' (typed @HexState % #fontInfos % at' fontNr) (Just fontInfo)

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

  currentFontNumber :: HexStateT m DVI.FontNumber
  currentFontNumber = currentFontNumberImpl

  currentFontSpaceGlue :: (HexStateT m) (Maybe Q.Glue)
  currentFontSpaceGlue = do
    fInfo <- currentFontInfo
    let fontMetrics = fInfo.fontMetrics
    let spacing = TFM.fontLengthParamLength fontMetrics (TFM.spacing . TFM.params)
    let gStretch = Q.FinitePureFlex $ TFM.fontLengthParamLength fontMetrics (TFM.spaceStretch . TFM.params)
    let gShrink = Q.FinitePureFlex $ TFM.fontLengthParamLength fontMetrics (TFM.spaceShrink . TFM.params)
    pure $ Just $ Q.Glue {Q.gDimen = spacing, Q.gStretch, Q.gShrink}

  currentFontCharacter :: Code.CharCode -> HexStateT m (Maybe (Q.Length, Q.Length, Q.Length, Q.Length))
  currentFontCharacter chrCode = do
    fInfo <- currentFontInfo
    let fontMetrics = fInfo.fontMetrics
    tfmChar <-
      note (injectTyped CharacterCodeNotFound) $
        fontMetrics ^. #characters % at' (Code.codeInt chrCode)
    let toLen :: TFM.LengthDesignSize -> Q.Length
        toLen = TFM.lengthFromFontDesignSize fontMetrics
    pure $
      Just
        ( toLen (tfmChar.width),
          toLen (tfmChar.height),
          toLen (tfmChar.depth),
          toLen (tfmChar.italicCorrection)
        )

  selectFont :: DVI.FontNumber -> HSt.Grouped.ScopeFlag -> HexStateT m ()
  selectFont fontNumber scopeFlag =
    modifyGroupScopes $ Sc.Font.setCurrentFontNr fontNumber scopeFlag

  setFamilyMemberFont :: HSt.Font.FamilyMember -> DVI.FontNumber -> HSt.Grouped.ScopeFlag -> HexStateT m ()
  setFamilyMemberFont familyMember fontNumber scopeFlag =
    modifyGroupScopes $ Sc.Font.setFamilyMemberFont familyMember fontNumber scopeFlag

  setFontSpecialCharacter :: HSt.Font.FontSpecialChar -> DVI.FontNumber -> Q.HexInt -> HexStateT m ()
  setFontSpecialCharacter fontSpecialChar fontNumber value = do
    let fontSpecialCharLens = case fontSpecialChar of
          HSt.Font.HyphenChar -> #hyphenChar
          HSt.Font.SkewChar -> #skewChar
    use (typed @HexState % #fontInfos % at' fontNumber) >>= \case
      Nothing -> throwError $ injectTyped MissingFontNumber
      Just _ -> pure ()
    assign' (typed @HexState % #fontInfos % at' fontNumber %? fontSpecialCharLens) value

  setAfterAssignmentToken :: LT.LexToken -> HexStateT m ()
  setAfterAssignmentToken t = assign' (typed @HexState % #afterAssignmentToken) (Just t)

  popAfterAssignmentToken :: HexStateT m (Maybe LT.LexToken)
  popAfterAssignmentToken = do
    v <- use (typed @HexState % #afterAssignmentToken)
    assign' (typed @HexState % #afterAssignmentToken) Nothing
    pure v

  pushGroup :: Maybe HSt.Grouped.ScopedGroupType -> HexStateT m ()
  pushGroup mayScopedGroupType = do
    debugLog "pushGroup"
    modifyGroupScopes $ Sc.GroupScopes.pushGroup mayScopedGroupType

  popGroup :: HSt.Grouped.ChangeGroupTrigger -> HexStateT m HSt.Grouped.HexGroupType
  popGroup exitTrigger = do
    use (typed @HexState % #groupScopes % to Sc.GroupScopes.popGroup) >>= \case
      Nothing -> throwError (injectTyped PoppedEmptyGroups)
      Just (poppedGroup, newGroupScopes) ->
        case poppedGroup of
          Sc.Group.ScopeGroup (Sc.Group.GroupScope _scope scopeGroupType) -> do
            assign' (typed @HexState % #groupScopes) newGroupScopes
            case scopeGroupType of
              HSt.Grouped.LocalStructureScopeGroup enterTrigger
                | exitTrigger == enterTrigger ->
                    pure HSt.Grouped.LocalStructureGroupType
                | otherwise ->
                    throwError (injectTyped UnmatchedExitGroupTrigger)
              HSt.Grouped.ExplicitBoxScopeGroup ->
                case exitTrigger of
                  HSt.Grouped.ChangeGroupCharTrigger ->
                    pure HSt.Grouped.ExplicitBoxGroupType
                  HSt.Grouped.ChangeGroupCSTrigger ->
                    throwError (injectTyped UnmatchedExitGroupTrigger)
          Sc.Group.NonScopeGroup ->
            notImplemented $ "popGroup: NonScopeGroup"

modifyGroupScopes :: (MonadState st m, HasType HexState st) => (GroupScopes -> GroupScopes) -> m ()
modifyGroupScopes = modifying' (typed @HexState % #groupScopes)

currentFontInfo ::
  ( MonadState st m,
    HasType HexState st,
    MonadError e m,
    AsType HexStateError e
  ) =>
  m Sc.Font.FontInfo
currentFontInfo =
  currentFontInfoImpl
    >>= note (injectTyped MissingFontNumber)

readFontInfo ::
  ( MonadIO m,
    MonadError e m,
    AsType TFM.TFMError e,
    MonadHexState m
  ) =>
  ByteString ->
  m Sc.Font.FontInfo
readFontInfo fontBytes = do
  fontMetrics <- TFM.parseTFMBytes fontBytes
  hyphenChar <- getParameterValue (HSt.Param.IntQuantParam HSt.Param.DefaultHyphenChar)
  skewChar <- getParameterValue (HSt.Param.IntQuantParam HSt.Param.DefaultSkewChar)
  pure Sc.Font.FontInfo {fontMetrics, hyphenChar, skewChar}

fetchBoxRegisterValueImpl ::
  ( MonadState st m,
    HasType HexState st
  ) =>
  HSt.Reg.BoxFetchMode ->
  HSt.Reg.RegisterLocation ->
  m (Maybe BoxElem.BaseBox)
fetchBoxRegisterValueImpl fetchMode loc = do
  mayV <- getGroupScopesProperty (Sc.R.localBoxRegisterValue loc)
  case fetchMode of
    HSt.Reg.Pop -> modifyGroupScopes $ Sc.R.unsetBoxRegisterValue loc HSt.Grouped.Local
    HSt.Reg.Lookup -> pure ()
  pure mayV

setBoxRegisterValueImpl ::
  ( MonadState st m,
    HasType HexState st
  ) =>
  HSt.Reg.RegisterLocation ->
  Maybe BoxElem.BaseBox ->
  HSt.Grouped.ScopeFlag ->
  m ()
setBoxRegisterValueImpl loc mayV scope =
  modifyGroupScopes $ case mayV of
    Nothing -> Sc.R.unsetBoxRegisterValue loc scope
    Just v -> Sc.R.setBoxRegisterValue loc v scope
