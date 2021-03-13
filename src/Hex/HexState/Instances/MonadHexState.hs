{-# LANGUAGE UndecidableInstances #-}
{-# OPTIONS_GHC -Wno-orphans #-}

module Hex.HexState.Instances.MonadHexState where

import Data.Map.Strict qualified as Map
import Data.Text qualified as Tx
import Hex.Codes qualified as H.Codes
import Hex.HexState.Type
import Hex.Lex.Types qualified as H.Lex
import Hex.MonadHexState.Interface
import Hex.Quantity qualified as H.Q
import Hex.Symbol.Tokens qualified as H.Sym.Tok
import Hex.TFM.Get qualified as H.TFM
import Hex.TFM.Types qualified as H.TFM
import Hexlude
import System.FilePath qualified as FilePath

data HexStateError
  = FontNotFound
  | BadPath Text
  | CharacterCodeNotFound
  deriving stock (Show, Generic)

instance (Monad m, MonadIO m, MonadState st m, HasType HexState st, MonadError e m, AsType HexStateError e, AsType H.TFM.TFMError e) => MonadHexState m where
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

  resolveSymbol :: H.Lex.LexSymbol -> m (Maybe H.Sym.Tok.ResolvedToken)
  resolveSymbol p = use $ typed @HexState % to (stateLocalResolvedToken p)

  currentFontSpaceGlue :: m (Maybe H.Q.Glue)
  currentFontSpaceGlue = do
    currentFontInfo >>= \case
      Nothing -> pure Nothing
      Just fInfo -> do
        let font = fontMetrics fInfo
        let spacing = H.TFM.fontLengthParamScaledPointsInt font (H.TFM.spacing . H.TFM.params)
        let gStretch = H.Q.FiniteFlex $ H.TFM.fontLengthParamScaledPointsInt font (H.TFM.spaceStretch . H.TFM.params)
        let gShrink = H.Q.FiniteFlex $ H.TFM.fontLengthParamScaledPointsInt font (H.TFM.spaceShrink . H.TFM.params)
        pure $ Just $ H.Q.Glue {H.Q.gDimen = spacing, H.Q.gStretch, H.Q.gShrink}

  currentFontCharacter :: H.Codes.CharCode -> m (Maybe (H.Q.Length, H.Q.Length, H.Q.Length, H.Q.Length))
  currentFontCharacter chrCode = do
    currentFontInfo >>= \case
      Nothing -> pure Nothing
      Just fInfo -> do
        let fontMetrics = fInfo ^. typed @H.TFM.Font
        tfmChar <- note (injectTyped CharacterCodeNotFound) $ fontMetrics ^. field @"characters" % at' (fromIntegral $ H.Codes.codeWord chrCode)
        let toLen :: H.Q.LengthDesignSize Rational -> H.Q.LengthScaledPoints Int
            toLen = H.TFM.fontLengthScaledPointsInt fontMetrics
        pure $ Just (tfmChar ^. field @"width" % to toLen, tfmChar ^. field @"height" % to toLen, tfmChar ^. field @"depth" % to toLen, tfmChar ^. field @"italicCorrection" % to toLen)

  loadFont :: FilePath -> m (FontNumber, Text)
  loadFont path = do
    fontInfo <- readFontInfo path
    mayLastKey <- use $ typed @HexState % field @"fontInfos" % to Map.lookupMax
    let newKey = case mayLastKey of
          Nothing -> FontNumber $ H.Q.HexInt 0
          Just (i, _) -> succ i
    assign' (typed @HexState % field @"fontInfos" % at' newKey) (Just fontInfo)

    let fontName = Tx.pack $ FilePath.takeBaseName path
    pure (newKey, fontName)

  selectFont :: FontNumber -> H.Sym.Tok.ScopeFlag -> m ()
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
