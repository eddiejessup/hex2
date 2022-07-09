{-# LANGUAGE UndecidableInstances #-}

module Hex.Common.HexState.Impl where

import Data.Map.Strict qualified as Map
import Data.Text qualified as Tx
import Formatting qualified as F
import Hex.Capability.Log.Interface (MonadHexLog (..))
import Hex.Common.Codes qualified as Code
import Hex.Common.HexEnv.Interface qualified as Env
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
import Hex.Stage.Build.BoxElem qualified as Box
import Hex.Stage.Build.BoxElem qualified as H.Inter.B.Box
import Hexlude
import System.FilePath qualified as FilePath

data HexStateError
  = FontNotFound Q.HexFilePath
  | MissingFontNumber
  | BadPath Text
  | CharacterCodeNotFound
  | PoppedEmptyGroups
  | UnmatchedExitGroupTrigger
  deriving stock (Show, Generic)

fmtHexStateError :: Fmt HexStateError
fmtHexStateError = F.shown

newtype MonadHexStateImplT m a = MonadHexStateImplT {unMonadHexStateImplT :: m a}
  deriving newtype
    ( Functor,
      Applicative,
      Monad,
      MonadIO,
      MonadState st,
      MonadReader r,
      MonadError e,
      MonadHexLog
    )

instance
  ( Monad m,
    MonadHexLog (MonadHexStateImplT m),
    MonadIO (MonadHexStateImplT m),
    MonadState st (MonadHexStateImplT m),
    HasType HexState st,
    MonadReader r (MonadHexStateImplT m),
    HasType [FilePath] r,
    MonadError e (MonadHexStateImplT m),
    AsType HexStateError e,
    AsType TFM.TFMError e
  ) =>
  MonadHexState (MonadHexStateImplT m)
  where
  getParameterValue :: HSt.Param.QuantParam q -> MonadHexStateImplT m (HSt.Var.QuantVariableTarget q)
  getParameterValue p = getGroupScopesProperty (Sc.P.localParameterValue p)

  setParameterValue :: HSt.Param.QuantParam q -> HSt.Var.QuantVariableTarget q -> HSt.Grouped.ScopeFlag -> MonadHexStateImplT m ()
  setParameterValue param value scopeFlag =
    modifyGroupScopes $ Sc.P.setParameterValue param value scopeFlag

  getRegisterValue :: HSt.Reg.QuantRegisterLocation q -> MonadHexStateImplT m (HSt.Var.QuantVariableTarget q)
  getRegisterValue r = getGroupScopesProperty (Sc.R.localQuantRegisterValue r)

  setQuantRegisterValue :: HSt.Reg.QuantRegisterLocation q -> (HSt.Var.QuantVariableTarget q) -> HSt.Grouped.ScopeFlag -> MonadHexStateImplT m ()
  setQuantRegisterValue param value scopeFlag =
    modifyGroupScopes $ Sc.R.setQuantRegisterValue param value scopeFlag

  fetchBoxRegisterValue :: HSt.Reg.BoxFetchMode -> HSt.Reg.RegisterLocation -> MonadHexStateImplT m (Maybe (Box.Box Box.BaseBoxContents))
  fetchBoxRegisterValue = fetchBoxRegisterValueImpl

  setBoxRegisterValue :: HSt.Reg.RegisterLocation -> Maybe (Box.Box Box.BaseBoxContents) -> HSt.Grouped.ScopeFlag -> MonadHexStateImplT m ()
  setBoxRegisterValue = setBoxRegisterValueImpl

  getSpecialIntParameter :: HSt.Param.SpecialIntParameter -> (MonadHexStateImplT m) Q.HexInt
  getSpecialIntParameter p = use $ typed @HexState % stateSpecialIntParamLens p

  getSpecialLengthParameter :: HSt.Param.SpecialLengthParameter -> (MonadHexStateImplT m) Q.Length
  getSpecialLengthParameter p = use $ typed @HexState % stateSpecialLengthParamLens p

  setSpecialIntParameter :: HSt.Param.SpecialIntParameter -> Q.HexInt -> (MonadHexStateImplT m) ()
  setSpecialIntParameter p v = assign' (typed @HexState % stateSpecialIntParamLens p) v

  setSpecialLengthParameter :: HSt.Param.SpecialLengthParameter -> Q.Length -> (MonadHexStateImplT m) ()
  setSpecialLengthParameter p v = assign' (typed @HexState % stateSpecialLengthParamLens p) v

  getHexCode :: Code.CCodeType c -> Code.CharCode -> (MonadHexStateImplT m) (HSt.Code.CodeTableTarget c)
  getHexCode t p = getGroupScopesProperty (Sc.Code.localHexCode t p)

  setHexCode :: Code.CCodeType c -> Code.CharCode -> HSt.Code.CodeTableTarget c -> HSt.Grouped.ScopeFlag -> MonadHexStateImplT m ()
  setHexCode t idxCode code scopeFlag = modifyGroupScopes $ Sc.Code.setHexCode t idxCode code scopeFlag

  resolveSymbol :: ControlSymbol -> (MonadHexStateImplT m) (Maybe ResolvedToken)
  resolveSymbol p = getGroupScopesProperty (Sc.Sym.localResolvedToken p)

  setSymbol :: ControlSymbol -> ResolvedToken -> HSt.Grouped.ScopeFlag -> MonadHexStateImplT m ()
  setSymbol symbol target scopeFlag =
    modifyGroupScopes $ Sc.Sym.setSymbol symbol target scopeFlag

  loadFont ::
    Q.HexFilePath ->
    H.Inter.B.Box.FontSpecification ->
    MonadHexStateImplT m H.Inter.B.Box.FontDefinition
  loadFont fontPath spec = do
    searchDirs <- know (typed @[FilePath])
    absPath <-
      Env.findFilePath
        (Env.WithImplicitExtension "tfm")
        searchDirs
        (fontPath ^. typed @FilePath)
        >>= note (injectTyped (FontNotFound fontPath))

    fontInfo <- readFontInfo absPath

    let designScale = H.Inter.B.Box.fontSpecToDesignScale fontInfo.fontMetrics.designFontSize spec

    mayLastKey <- use $ typed @HexState % #fontInfos % to Map.lookupMax
    let fontNr = case mayLastKey of
          Nothing -> HSt.Font.FontNumber $ Q.HexInt 0
          Just (i, _) -> succ i
    assign' (typed @HexState % #fontInfos % at' fontNr) (Just fontInfo)

    let fontName = Tx.pack $ FilePath.takeBaseName absPath
    pure
      H.Inter.B.Box.FontDefinition
        { fontDefChecksum = fontInfo.fontMetrics.checksum,
          fontDefDesignSize = fontInfo.fontMetrics.designFontSize,
          fontDefDesignScale = designScale,
          fontNr,
          fontPath,
          fontName
        }

  currentFontNumber :: MonadHexStateImplT m HSt.Font.FontNumber
  currentFontNumber = currentFontNumberImpl

  currentFontSpaceGlue :: (MonadHexStateImplT m) (Maybe Q.Glue)
  currentFontSpaceGlue = do
    fInfo <- currentFontInfo
    let fontMetrics = fInfo.fontMetrics
    let spacing = TFM.fontLengthParamLength fontMetrics (TFM.spacing . TFM.params)
    let gStretch = Q.FinitePureFlex $ TFM.fontLengthParamLength fontMetrics (TFM.spaceStretch . TFM.params)
    let gShrink = Q.FinitePureFlex $ TFM.fontLengthParamLength fontMetrics (TFM.spaceShrink . TFM.params)
    pure $ Just $ Q.Glue {Q.gDimen = spacing, Q.gStretch, Q.gShrink}

  currentFontCharacter :: Code.CharCode -> MonadHexStateImplT m (Maybe (Q.Length, Q.Length, Q.Length, Q.Length))
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

  selectFont :: HSt.Font.FontNumber -> HSt.Grouped.ScopeFlag -> MonadHexStateImplT m ()
  selectFont fontNumber scopeFlag =
    modifyGroupScopes $ Sc.Font.setCurrentFontNr fontNumber scopeFlag

  setFamilyMemberFont :: HSt.Font.FamilyMember -> HSt.Font.FontNumber -> HSt.Grouped.ScopeFlag -> MonadHexStateImplT m ()
  setFamilyMemberFont familyMember fontNumber scopeFlag =
    modifyGroupScopes $ Sc.Font.setFamilyMemberFont familyMember fontNumber scopeFlag

  setFontSpecialCharacter :: HSt.Font.FontSpecialChar -> HSt.Font.FontNumber -> Q.HexInt -> MonadHexStateImplT m ()
  setFontSpecialCharacter fontSpecialChar fontNumber value = do
    let fontSpecialCharLens = case fontSpecialChar of
          HSt.Font.HyphenChar -> #hyphenChar
          HSt.Font.SkewChar -> #skewChar
    use (typed @HexState % #fontInfos % at' fontNumber) >>= \case
      Nothing -> throwError $ injectTyped MissingFontNumber
      Just _ -> pure ()
    assign' (typed @HexState % #fontInfos % at' fontNumber %? fontSpecialCharLens) value

  setAfterAssignmentToken :: LT.LexToken -> MonadHexStateImplT m ()
  setAfterAssignmentToken t = assign' (typed @HexState % #afterAssignmentToken) (Just t)

  popAfterAssignmentToken :: MonadHexStateImplT m (Maybe LT.LexToken)
  popAfterAssignmentToken = do
    v <- use (typed @HexState % #afterAssignmentToken)
    assign' (typed @HexState % #afterAssignmentToken) Nothing
    pure v

  pushGroup :: Maybe HSt.Grouped.ScopedGroupType -> MonadHexStateImplT m ()
  pushGroup mayScopedGroupType = do
    log "pushGroup"
    modifyGroupScopes $ Sc.GroupScopes.pushGroup mayScopedGroupType

  popGroup :: HSt.Grouped.ChangeGroupTrigger -> MonadHexStateImplT m HSt.Grouped.HexGroupType
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
  FilePath ->
  m Sc.Font.FontInfo
readFontInfo fontPath = do
  fontMetrics <- TFM.parseTFMFile fontPath
  hyphenChar <- getParameterValue (HSt.Param.IntQuantParam HSt.Param.DefaultHyphenChar)
  skewChar <- getParameterValue (HSt.Param.IntQuantParam HSt.Param.DefaultSkewChar)
  pure Sc.Font.FontInfo {fontMetrics, hyphenChar, skewChar}

fetchBoxRegisterValueImpl ::
  ( MonadState st m,
    HasType HexState st
  ) =>
  HSt.Reg.BoxFetchMode ->
  HSt.Reg.RegisterLocation ->
  m (Maybe (Box.Box Box.BaseBoxContents))
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
  Maybe (Box.Box Box.BaseBoxContents) ->
  HSt.Grouped.ScopeFlag ->
  m ()
setBoxRegisterValueImpl loc mayV scope =
  modifyGroupScopes $ case mayV of
    Nothing -> Sc.R.unsetBoxRegisterValue loc scope
    Just v -> Sc.R.setBoxRegisterValue loc v scope
