{-# LANGUAGE UndecidableInstances #-}

module Hex.Common.HexState.Impl where

import Data.Map.Strict qualified as Map
import Data.Text qualified as Tx
import Formatting qualified as F
import Hex.Capability.Log.Interface (MonadHexLog (..))
import Hex.Common.Codes qualified as Code
import Hex.Common.HexEnv.Interface qualified as Env
import Hex.Common.HexState.Impl.Font qualified as HSt.Font
import Hex.Common.HexState.Impl.Scoped.Code qualified as Sc.C
import Hex.Common.HexState.Impl.Scoped.Font qualified as Sc.Font
import Hex.Common.HexState.Impl.Scoped.Group qualified as Group
import Hex.Common.HexState.Impl.Scoped.GroupScopes (GroupScopes)
import Hex.Common.HexState.Impl.Scoped.GroupScopes qualified as GroupScopes
import Hex.Common.HexState.Impl.Scoped.Parameter qualified as Sc.P
import Hex.Common.HexState.Impl.Scoped.Register qualified as Sc.R
import Hex.Common.HexState.Impl.Scoped.Symbol qualified as Sc.Sym
import Hex.Common.HexState.Impl.Type
import Hex.Common.HexState.Interface
import Hex.Common.HexState.Interface.Code qualified as HSt.Code
import Hex.Common.HexState.Interface.Grouped qualified as Grouped
import Hex.Common.HexState.Interface.Grouped qualified as HSt.Grouped
import Hex.Common.HexState.Interface.Parameter qualified as Param
import Hex.Common.HexState.Interface.Register qualified as Reg
import Hex.Common.HexState.Interface.Resolve (ControlSymbol, ResolvedToken)
import Hex.Common.HexState.Interface.Resolve.PrimitiveToken qualified as PT
import Hex.Common.HexState.Interface.Variable qualified as Var
import Hex.Common.Quantity qualified as Q
import Hex.Common.TFM.Get qualified as TFM
import Hex.Common.TFM.Types qualified as TFM
import Hex.Stage.Interpret.Build.Box.Elem qualified as H.Inter.B.Box
import Hex.Stage.Lex.Interface.Extract qualified as Lex
import Hexlude
import System.FilePath qualified as FilePath

data HexStateError
  = FontNotFound Q.HexFilePath
  | MissingFontNumber
  | BadPath Text
  | CharacterCodeNotFound
  | PoppedEmptyGroups
  | UnmatchedLocalStructureTrigger
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
  ( Monad (MonadHexStateImplT m),
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
  getParameterValue :: (Param.QuantParam q) -> (MonadHexStateImplT m) (Var.QuantVariableTarget q)
  getParameterValue p = getGroupScopesProperty (Sc.P.localParameterValue p)

  setParameterValue :: (Param.QuantParam q) -> Var.QuantVariableTarget q -> PT.ScopeFlag -> MonadHexStateImplT m ()
  setParameterValue param value scopeFlag =
    modifyGroupScopes $ Sc.P.setParameterValue param value scopeFlag

  getRegisterValue :: Reg.QuantRegisterLocation q -> MonadHexStateImplT m (Var.QuantVariableTarget q)
  getRegisterValue r = getGroupScopesProperty (Sc.R.localRegisterValue r)

  setRegisterValue :: Reg.QuantRegisterLocation q -> (Var.QuantVariableTarget q) -> PT.ScopeFlag -> MonadHexStateImplT m ()
  setRegisterValue param value scopeFlag = do
    modifyGroupScopes $ Sc.R.setRegisterValue param value scopeFlag

  getSpecialIntParameter :: Param.SpecialIntParameter -> (MonadHexStateImplT m) Q.HexInt
  getSpecialIntParameter p = use $ typed @HexState % stateSpecialIntParamLens p

  getSpecialLengthParameter :: Param.SpecialLengthParameter -> (MonadHexStateImplT m) Q.Length
  getSpecialLengthParameter p = use $ typed @HexState % stateSpecialLengthParamLens p

  setSpecialIntParameter :: Param.SpecialIntParameter -> Q.HexInt -> (MonadHexStateImplT m) ()
  setSpecialIntParameter p v = assign' (typed @HexState % stateSpecialIntParamLens p) v

  setSpecialLengthParameter :: Param.SpecialLengthParameter -> Q.Length -> (MonadHexStateImplT m) ()
  setSpecialLengthParameter p v = assign' (typed @HexState % stateSpecialLengthParamLens p) v

  getHexCode :: Code.CCodeType c -> Code.CharCode -> (MonadHexStateImplT m) (HSt.Code.CodeTableTarget c)
  getHexCode t p = getGroupScopesProperty (Sc.C.localHexCode t p)

  setHexCode :: Code.CCodeType c -> Code.CharCode -> HSt.Code.CodeTableTarget c -> PT.ScopeFlag -> MonadHexStateImplT m ()
  setHexCode t idxCode code scopeFlag = modifyGroupScopes $ Sc.C.setHexCode t idxCode code scopeFlag

  resolveSymbol :: ControlSymbol -> (MonadHexStateImplT m) (Maybe ResolvedToken)
  resolveSymbol p = getGroupScopesProperty (Sc.Sym.localResolvedToken p)

  setSymbol :: ControlSymbol -> ResolvedToken -> PT.ScopeFlag -> MonadHexStateImplT m ()
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
          Nothing -> PT.FontNumber $ Q.HexInt 0
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

  currentFontNumber :: MonadHexStateImplT m PT.FontNumber
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

  selectFont :: PT.FontNumber -> PT.ScopeFlag -> MonadHexStateImplT m ()
  selectFont fontNumber scopeFlag =
    modifyGroupScopes $ Sc.Font.setCurrentFontNr fontNumber scopeFlag

  setFontSpecialCharacter :: PT.FontSpecialChar -> PT.FontNumber -> Q.HexInt -> MonadHexStateImplT m ()
  setFontSpecialCharacter fontSpecialChar fontNumber value = do
    let fontSpecialCharLens = case fontSpecialChar of
          PT.HyphenChar -> #hyphenChar
          PT.SkewChar -> #skewChar
    use (typed @HexState % #fontInfos % at' fontNumber) >>= \case
      Nothing -> throwError $ injectTyped MissingFontNumber
      Just _ -> pure ()
    assign' (typed @HexState % #fontInfos % at' fontNumber %? fontSpecialCharLens) value

  setAfterAssignmentToken :: Lex.LexToken -> MonadHexStateImplT m ()
  setAfterAssignmentToken t = assign' (typed @HexState % #afterAssignmentToken) (Just t)

  popAfterAssignmentToken :: MonadHexStateImplT m (Maybe Lex.LexToken)
  popAfterAssignmentToken = do
    v <- use (typed @HexState % #afterAssignmentToken)
    assign' (typed @HexState % #afterAssignmentToken) Nothing
    pure v

  pushGroup :: Maybe Grouped.ScopedGroupType -> MonadHexStateImplT m ()
  pushGroup mayScopedGroupType = modifyGroupScopes $ GroupScopes.pushGroup mayScopedGroupType

  popGroup :: HSt.Grouped.LocalStructureTrigger -> MonadHexStateImplT m ()
  popGroup exitTrigger = do
    use (typed @HexState % #groupScopes % to GroupScopes.popGroup) >>= \case
      Nothing -> throwError (injectTyped PoppedEmptyGroups)
      Just (poppedGroup, newGroupScopes) -> do
        case poppedGroup of
          Group.ScopeGroup (Group.GroupScope _scope (Grouped.LocalStructureScopeGroup entryTrigger))
            | entryTrigger == exitTrigger ->
                assign' (typed @HexState % #groupScopes) newGroupScopes
          _ ->
            throwError (injectTyped (UnmatchedLocalStructureTrigger))

modifyGroupScopes :: (MonadState st m, HasType HexState st) => (GroupScopes -> GroupScopes) -> m ()
modifyGroupScopes = modifying' (typed @HexState % #groupScopes)

currentFontInfo ::
  ( MonadState st m,
    HasType HexState st,
    MonadError e m,
    AsType HexStateError e
  ) =>
  m HSt.Font.FontInfo
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
  m HSt.Font.FontInfo
readFontInfo fontPath = do
  fontMetrics <- TFM.parseTFMFile fontPath
  hyphenChar <- getParameterValue (Param.IntQuantParam Param.DefaultHyphenChar)
  skewChar <- getParameterValue (Param.IntQuantParam Param.DefaultSkewChar)
  pure HSt.Font.FontInfo {fontMetrics, hyphenChar, skewChar}
