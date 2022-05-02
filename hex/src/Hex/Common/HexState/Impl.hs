{-# OPTIONS_GHC -Wno-orphans #-}
{-# LANGUAGE UndecidableInstances #-}

module Hex.Common.HexState.Impl where

import Data.Map.Strict qualified as Map
import Data.Text qualified as Tx
import Hex.Common.Codes qualified as H.Codes
import Hex.Common.HexState.Impl.Type
import Hex.Common.HexState.Interface
import Hex.Common.HexState.Interface.Resolve (ControlSymbol, ResolvedToken)
import Hex.Common.HexState.Interface.Resolve.PrimitiveToken qualified as PT
import Hex.Common.Quantity qualified as H.Q
import Hex.Common.TFM.Get qualified as H.TFM
import Hex.Common.TFM.Types qualified as H.TFM
import Hex.Stage.Interpret.Build.Box.Elem qualified as H.Inter.B.Box
import Hexlude
import System.FilePath qualified as FilePath
import qualified Hex.Stage.Lex.Interface.Extract as Lex

data HexStateError
  = FontNotFound
  | BadPath Text
  | CharacterCodeNotFound
  deriving stock (Show, Generic)

instance (Monad m
         , MonadIO m
         , MonadState st m
         , HasType HexState st
         , MonadError e m
         , AsType HexStateError e
         , AsType H.TFM.TFMError e
         ) => MonadHexState m where
  getIntParameter :: PT.IntParameter -> m H.Q.HexInt
  getIntParameter p = use $ typed @HexState % to (stateLocalIntParam p)

  getLengthParameter :: PT.LengthParameter -> m H.Q.Length
  getLengthParameter p = use $ typed @HexState % to (stateLocalLengthParam p)

  getGlueParameter :: PT.GlueParameter -> m H.Q.Glue
  getGlueParameter p = use $ typed @HexState % to (stateLocalGlueParam p)

  getSpecialLengthParameter :: PT.SpecialLengthParameter -> m H.Q.Length
  getSpecialLengthParameter p = use $ typed @HexState % stateSpecialLengthParamLens p

  setSpecialLengthParameter :: PT.SpecialLengthParameter -> H.Q.Length -> m ()
  setSpecialLengthParameter p v = assign' (typed @HexState % stateSpecialLengthParamLens p) v

  getCategory :: H.Codes.CharCode -> m H.Codes.CatCode
  getCategory p = use $ typed @HexState % to (stateLocalCategory p)

  resolveSymbol :: ControlSymbol -> m (Maybe ResolvedToken)
  resolveSymbol p = use $ typed @HexState % to (stateLocalResolvedToken p)

  currentFontSpaceGlue :: m (Maybe H.Q.Glue)
  currentFontSpaceGlue = do
    currentFontInfo >>= \case
      Nothing -> pure Nothing
      Just fInfo -> do
        let font = fontMetrics fInfo
        let spacing = H.TFM.fontLengthParamScaledPointsInt font (H.TFM.spacing . H.TFM.params)
        let gStretch = H.Q.FinitePureFlex $ H.TFM.fontLengthParamScaledPointsInt font (H.TFM.spaceStretch . H.TFM.params)
        let gShrink = H.Q.FinitePureFlex $ H.TFM.fontLengthParamScaledPointsInt font (H.TFM.spaceShrink . H.TFM.params)
        pure $ Just $ H.Q.Glue {H.Q.gDimen = spacing, H.Q.gStretch, H.Q.gShrink}

  currentFontCharacter :: H.Codes.CharCode -> m (Maybe (H.Q.Length, H.Q.Length, H.Q.Length, H.Q.Length))
  currentFontCharacter chrCode = do
    currentFontInfo >>= \case
      Nothing -> pure Nothing
      Just fInfo -> do
        let fontMetrics = fInfo ^. typed @H.TFM.Font
        tfmChar <- note (injectTyped CharacterCodeNotFound) $ fontMetrics ^. #characters % at' (fromIntegral $ H.Codes.unCharCode chrCode)
        let toLen :: H.Q.LengthDesignSize Rational -> H.Q.LengthScaledPoints Int
            toLen = H.TFM.fontLengthScaledPointsInt fontMetrics
        pure $ Just (tfmChar ^. #width % to toLen, tfmChar ^. #height % to toLen, tfmChar ^. #depth % to toLen, tfmChar ^. #italicCorrection % to toLen)

  loadFont :: H.Inter.B.Box.HexFilePath -> H.Inter.B.Box.FontSpecification -> m H.Inter.B.Box.FontDefinition
  loadFont path spec = do
    let filePath = path ^. typed @FilePath
    fontInfo <- readFontInfo filePath
    case spec of
      H.Inter.B.Box.NaturalFont -> pure ()
      H.Inter.B.Box.FontAt _ -> panic "not implemented: font-at"
      H.Inter.B.Box.FontScaled _ -> panic "not implemented: font-scaled"
    mayLastKey <- use $ typed @HexState % #fontInfos % to Map.lookupMax
    let newKey = case mayLastKey of
          Nothing -> PT.FontNumber $ H.Q.HexInt 0
          Just (i, _) -> succ i
    assign' (typed @HexState % #fontInfos % at' newKey) (Just fontInfo)

    let fontName = Tx.pack $ FilePath.takeBaseName filePath
    pure
      H.Inter.B.Box.FontDefinition
        { H.Inter.B.Box.fontDefChecksum = panic "Not implemented",
          H.Inter.B.Box.fontDefDesignSize = panic "Not implemented",
          H.Inter.B.Box.fontDefDesignScale = panic "Not implemented",
          H.Inter.B.Box.fontNr = newKey,
          H.Inter.B.Box.fontPath = path,
          H.Inter.B.Box.fontName = fontName
        }

  selectFont :: PT.FontNumber -> PT.ScopeFlag -> m ()
  selectFont fNr scopeFlag = modifying' (typed @HexState) (selectFontNr fNr scopeFlag)

  setLastFetchedLexTok :: Lex.LexToken -> m ()
  setLastFetchedLexTok t =
    assign' (typed @HexState % #lastFetchedLexTok) (Just t)

  getLastFetchedLexTok :: m (Maybe Lex.LexToken)
  getLastFetchedLexTok =
    use (typed @HexState % #lastFetchedLexTok)

  getResolutionMode = use (typed @HexState % #resolutionMode)

  setResolutionMode = assign' (typed @HexState % #resolutionMode)

  setAfterAssignmentToken = panic "Not implemented"
  setControlSequence = panic "Not implemented"

currentFontInfo ::
  ( MonadState st m,
    HasType HexState st,
    MonadError e m,
    AsType HexStateError e
  ) =>
  m (Maybe FontInfo)
currentFontInfo = do
  use (typed @HexState % to stateCurrentFontNr) >>= \case
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
    AsType H.TFM.TFMError e,
    MonadHexState m
  ) =>
  FilePath ->
  m FontInfo
readFontInfo fontPath = do
  fontMetrics <- H.TFM.parseTFMFile fontPath
  hyphenChar <- getIntParameter PT.DefaultHyphenChar
  skewChar <- getIntParameter PT.DefaultSkewChar
  pure FontInfo {fontMetrics, hyphenChar, skewChar}
