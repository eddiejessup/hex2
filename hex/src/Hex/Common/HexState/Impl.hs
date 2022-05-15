{-# LANGUAGE UndecidableInstances #-}

module Hex.Common.HexState.Impl where

import Data.Map.Strict qualified as Map
import Data.Text qualified as Tx
import Formatting qualified as F
import Hex.Capability.Log.Interface (MonadHexLog (..))
import Hex.Common.Codes qualified as Code
import Hex.Common.HexState.Impl.Font qualified as HSt.Font
import Hex.Common.HexState.Impl.Scoped.Code (MutableHexCode)
import Hex.Common.HexState.Impl.Scoped.Code qualified as Sc.C
import Hex.Common.HexState.Impl.Scoped.Font qualified as Sc.Font
import Hex.Common.HexState.Impl.Scoped.GroupScopes (GroupScopes)
import Hex.Common.HexState.Impl.Scoped.GroupScopes qualified as GroupScopes
import Hex.Common.HexState.Impl.Scoped.Parameter qualified as Sc.P
import Hex.Common.HexState.Impl.Scoped.Register qualified as Sc.R
import Hex.Common.HexState.Impl.Scoped.Symbol qualified as Sc.Sym
import Hex.Common.HexState.Impl.Type
import Hex.Common.HexState.Interface
import Hex.Common.HexState.Interface.Grouped qualified as Group
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
  = FontNotFound
  | BadPath Text
  | CharacterCodeNotFound
  | PoppedEmptyGroups
  deriving stock (Show, Generic)

fmtHexStateError :: Fmt HexStateError
fmtHexStateError = F.shown

getGroupScopesProperty ::
  (MonadState st m, HasType HexState st) => (GroupScopes -> a) -> m a
getGroupScopesProperty groupScopesGetter =
  use $ typed @HexState % #groupScopes % to groupScopesGetter

newtype MonadHexStateImplT m a = MonadHexStateImplT {unMonadHexStateImplT :: m a}
  deriving newtype
    ( Functor,
      Applicative,
      Monad,
      MonadIO,
      MonadState st,
      MonadError e,
      MonadHexLog
    )

instance
  ( Monad (MonadHexStateImplT m),
    MonadHexLog (MonadHexStateImplT m),
    MonadIO (MonadHexStateImplT m),
    MonadState st (MonadHexStateImplT m),
    HasType HexState st,
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

  getHexCode :: MutableHexCode c => Code.CharCode -> (MonadHexStateImplT m) c
  getHexCode p = getGroupScopesProperty (Sc.C.localHexCode p)

  setHexCode :: MutableHexCode c => Code.CharCode -> c -> PT.ScopeFlag -> MonadHexStateImplT m ()
  setHexCode idxCode code scopeFlag = do
    -- logText $ sformat ("setHexCode: " |%| Code.fmtCharCode |%| " -> " |%| F.shown) idxCode code
    modifyGroupScopes $ Sc.C.setHexCode idxCode code scopeFlag

  resolveSymbol :: ControlSymbol -> (MonadHexStateImplT m) (Maybe ResolvedToken)
  resolveSymbol p = getGroupScopesProperty (Sc.Sym.localResolvedToken p)

  setSymbol :: ControlSymbol -> ResolvedToken -> PT.ScopeFlag -> MonadHexStateImplT m ()
  setSymbol symbol target scopeFlag =
    modifyGroupScopes $ Sc.Sym.setSymbol symbol target scopeFlag

  currentFontSpaceGlue :: (MonadHexStateImplT m) (Maybe Q.Glue)
  currentFontSpaceGlue = do
    currentFontInfo >>= \case
      Nothing -> pure Nothing
      Just fInfo -> do
        let font = HSt.Font.fontMetrics fInfo
        let spacing = TFM.fontLengthParamLength font (TFM.spacing . TFM.params)
        let gStretch = Q.FinitePureFlex $ TFM.fontLengthParamLength font (TFM.spaceStretch . TFM.params)
        let gShrink = Q.FinitePureFlex $ TFM.fontLengthParamLength font (TFM.spaceShrink . TFM.params)
        pure $ Just $ Q.Glue {Q.gDimen = spacing, Q.gStretch, Q.gShrink}

  currentFontCharacter :: Code.CharCode -> MonadHexStateImplT m (Maybe (Q.Length, Q.Length, Q.Length, Q.Length))
  currentFontCharacter chrCode = do
    currentFontInfo >>= \case
      Nothing -> pure Nothing
      Just fInfo -> do
        let fontMetrics = HSt.Font.fontMetrics fInfo
        tfmChar <- note (injectTyped CharacterCodeNotFound) $ fontMetrics ^. #characters % at' (fromIntegral $ Code.unCharCode chrCode)
        let toLen :: TFM.LengthDesignSize -> Q.Length
            toLen = TFM.lengthFromFontDesignSize fontMetrics
        pure $
          Just
            ( tfmChar ^. #width % to toLen,
              tfmChar ^. #height % to toLen,
              tfmChar ^. #depth % to toLen,
              tfmChar ^. #italicCorrection % to toLen
            )

  loadFont ::
    Q.HexFilePath ->
    H.Inter.B.Box.FontSpecification ->
    MonadHexStateImplT m H.Inter.B.Box.FontDefinition
  loadFont path spec = do
    let filePath = path ^. typed @FilePath
    fontInfo <- readFontInfo filePath
    case spec of
      H.Inter.B.Box.NaturalFont -> pure ()
      H.Inter.B.Box.FontAt _ -> notImplemented "font-at"
      H.Inter.B.Box.FontScaled _ -> notImplemented "font-scaled"
    mayLastKey <- use $ typed @HexState % #fontInfos % to Map.lookupMax
    let newKey = case mayLastKey of
          Nothing -> PT.FontNumber $ Q.HexInt 0
          Just (i, _) -> succ i
    assign' (typed @HexState % #fontInfos % at' newKey) (Just fontInfo)

    let fontName = Tx.pack $ FilePath.takeBaseName filePath
    pure
      H.Inter.B.Box.FontDefinition
        { H.Inter.B.Box.fontDefChecksum = notImplemented "loadFont: Checksum",
          H.Inter.B.Box.fontDefDesignSize = notImplemented "loadFont: DesignSize",
          H.Inter.B.Box.fontDefDesignScale = notImplemented "loadFont: DesignScale",
          H.Inter.B.Box.fontNr = newKey,
          H.Inter.B.Box.fontPath = path,
          H.Inter.B.Box.fontName = fontName
        }

  selectFont :: PT.FontNumber -> PT.ScopeFlag -> MonadHexStateImplT m ()
  selectFont fNr scopeFlag =
    modifyGroupScopes $ Sc.Font.setCurrentFontNr fNr scopeFlag

  setAfterAssignmentToken :: Lex.LexToken -> MonadHexStateImplT m ()
  setAfterAssignmentToken t = assign' (typed @HexState % #afterAssignmentToken) (Just t)

  popAfterAssignmentToken :: MonadHexStateImplT m (Maybe Lex.LexToken)
  popAfterAssignmentToken = do
    v <- use (typed @HexState % #afterAssignmentToken)
    assign' (typed @HexState % #afterAssignmentToken) Nothing
    pure v

  pushGroup :: Maybe Group.ScopedGroupType -> MonadHexStateImplT m ()
  pushGroup mayScopedGroupType = modifyGroupScopes $ GroupScopes.pushGroup mayScopedGroupType

  popGroup :: MonadHexStateImplT m ()
  popGroup = do
    use (typed @HexState % #groupScopes % to GroupScopes.popGroup) >>= \case
      Nothing -> throwError (injectTyped PoppedEmptyGroups)
      Just (_poppedGroup, newGroupScopes) -> do
        assign' (typed @HexState % #groupScopes) newGroupScopes

modifyGroupScopes :: (MonadState st m, HasType HexState st) => (GroupScopes -> GroupScopes) -> m ()
modifyGroupScopes = modifying' (typed @HexState % #groupScopes)

currentFontInfo ::
  ( MonadState st m,
    HasType HexState st,
    MonadError e m,
    AsType HexStateError e
  ) =>
  m (Maybe HSt.Font.FontInfo)
currentFontInfo = do
  getGroupScopesProperty Sc.Font.localCurrentFontNr >>= \case
    -- No set font number is a Nothing.
    Nothing -> pure Nothing
    Just fNr -> do
      -- But a set font number that's absent from the fontInfo map is a proper
      -- error.
      use (typed @HexState % stateFontInfoLens fNr) >>= \case
        Nothing -> throwError (injectTyped FontNotFound)
        x -> pure x

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
