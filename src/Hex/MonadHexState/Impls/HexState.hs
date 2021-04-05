{-# LANGUAGE UndecidableInstances #-}
{-# OPTIONS_GHC -Wno-orphans #-}

module Hex.MonadHexState.Impls.HexState where

import Data.Map.Strict qualified as Map
import Data.Text qualified as Tx
import Hex.Codes qualified as H.Codes
import Hex.Interpret.Build.Box.Elem qualified as H.Inter.B.Box
import Hex.MonadHexState.Impls.HexState.Type
import Hex.MonadHexState.Interface
import Hex.Quantity qualified as H.Q
import Hex.Symbol.Token.Primitive qualified as H.Sym.Tok
import Hex.Symbol.Token.Resolved qualified as H.Sym.Tok
import Hex.Symbol.Types qualified as H.Sym
import Hex.TFM.Get qualified as H.TFM
import Hex.TFM.Types qualified as H.TFM
import Hexlude
import System.FilePath qualified as FilePath

data HexStateError
  = FontNotFound
  | BadPath Text
  | CharacterCodeNotFound
  deriving stock (Show, Generic)

instance {-# OVERLAPPING #-} (Monad m, MonadIO m, MonadState st m, HasType HexState st, MonadError e m, AsType HexStateError e, AsType H.TFM.TFMError e) => MonadHexState m where
  getIntParameter :: H.Sym.Tok.IntParameter -> m H.Q.HexInt
  getIntParameter p = use $ typed @HexState % to (stateLocalIntParam p)

  getLengthParameter :: H.Sym.Tok.LengthParameter -> m H.Q.Length
  getLengthParameter p = use $ typed @HexState % to (stateLocalLengthParam p)

  getGlueParameter :: H.Sym.Tok.GlueParameter -> m H.Q.Glue
  getGlueParameter p = use $ typed @HexState % to (stateLocalGlueParam p)

  getSpecialLengthParameter :: H.Sym.Tok.SpecialLengthParameter -> m H.Q.Length
  getSpecialLengthParameter p = use $ typed @HexState % stateSpecialLengthParamLens p

  setSpecialLengthParameter :: H.Sym.Tok.SpecialLengthParameter -> H.Q.Length -> m ()
  setSpecialLengthParameter p v = assign' (typed @HexState % stateSpecialLengthParamLens p) v

  getCategory :: H.Codes.CharCode -> m H.Codes.CatCode
  getCategory p = use $ typed @HexState % to (stateLocalCategory p)

  resolveSymbol :: H.Sym.ControlSymbol -> m (Maybe H.Sym.Tok.ResolvedToken)
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
          Nothing -> H.Sym.Tok.FontNumber $ H.Q.HexInt 0
          Just (i, _) -> succ i
    assign' (typed @HexState % #fontInfos % at' newKey) (Just fontInfo)

    let fontName = Tx.pack $ FilePath.takeBaseName filePath
    pure
      H.Inter.B.Box.FontDefinition
        { H.Inter.B.Box.fontDefChecksum = undefined,
          H.Inter.B.Box.fontDefDesignSize = undefined,
          H.Inter.B.Box.fontDefDesignScale = undefined,
          H.Inter.B.Box.fontNr = newKey,
          H.Inter.B.Box.fontPath = path,
          H.Inter.B.Box.fontName = fontName
        }

  selectFont :: H.Sym.Tok.FontNumber -> H.Sym.Tok.ScopeFlag -> m ()
  selectFont fNr scopeFlag = modifying' (typed @HexState) (selectFontNr fNr scopeFlag)

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
  hyphenChar <- getIntParameter H.Sym.Tok.DefaultHyphenChar
  skewChar <- getIntParameter H.Sym.Tok.DefaultSkewChar
  pure FontInfo {fontMetrics, hyphenChar, skewChar}
