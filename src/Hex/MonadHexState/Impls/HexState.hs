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
import qualified Hex.Syntax.Quantity as H.Syn
import qualified Hex.Syntax.Common as H.Syn
import qualified Hex.Syntax.Font as H.Syn
import Hex.MonadHexState.Impls.HexState.Scope qualified as H.St.Scope

data HexStateError
  = FontNotFound
  | BadPath Text
  | CharacterCodeNotFound
  deriving stock (Show, Generic)

instance {-# OVERLAPPING #-} (Monad m, MonadIO m, MonadState st m, HasType HexState st, MonadError e m, AsType HexStateError e, AsType H.TFM.TFMError e) => MonadHexState m where
  getIntVariable :: H.Syn.QuantVariable 'H.Syn.Evaluated 'H.Sym.Tok.IntQuantity -> m H.Q.HexInt
  getIntVariable v = use $ typed @HexState % to (stateLocalIntVar v)

  setIntVariable v scope tgt = case scope of
    H.Sym.Tok.Local -> assign' (typed @HexState % stateLocalMostScopeLens % H.St.Scope.scopeIntVarLens v) (Just tgt)
    H.Sym.Tok.Global -> panic "Not implemented: assign global int variable"

  getLengthVariable :: H.Syn.QuantVariable 'H.Syn.Evaluated 'H.Sym.Tok.LengthQuantity -> m H.Q.Length
  getLengthVariable v = use $ typed @HexState % to (stateLocalLengthVar v)

  setLengthVariable v scope tgt = case scope of
    H.Sym.Tok.Local -> assign' (typed @HexState % stateLocalMostScopeLens % H.St.Scope.scopeLengthVarLens v) (Just tgt)
    H.Sym.Tok.Global -> panic "Not implemented: assign global length variable"

  getGlueVariable :: H.Syn.QuantVariable 'H.Syn.Evaluated 'H.Sym.Tok.GlueQuantity -> m H.Q.Glue
  getGlueVariable v = use $ typed @HexState % to (stateLocalGlueVar v)

  setGlueVariable v scope tgt = case scope of
    H.Sym.Tok.Local -> assign' (typed @HexState % stateLocalMostScopeLens % H.St.Scope.scopeGlueVarLens v) (Just tgt)
    H.Sym.Tok.Global -> panic "Not implemented: assign global glue variable"

  getSpecialLengthParameter :: H.Sym.Tok.SpecialLengthParameter -> m H.Q.Length
  getSpecialLengthParameter p = use $ typed @HexState % stateSpecialLengthParamLens p

  setSpecialLengthParameter :: H.Sym.Tok.SpecialLengthParameter -> H.Q.Length -> m ()
  setSpecialLengthParameter p tgt = assign' (typed @HexState % stateSpecialLengthParamLens p) tgt

  getSpecialIntParameter :: H.Sym.Tok.SpecialIntParameter -> m H.Q.HexInt
  getSpecialIntParameter p = use $ typed @HexState % stateSpecialIntParamLens p

  setSpecialIntParameter :: H.Sym.Tok.SpecialIntParameter -> H.Q.HexInt -> m ()
  setSpecialIntParameter p tgt = assign' (typed @HexState % stateSpecialIntParamLens p) tgt

  getCategory :: H.Codes.CharCode -> m H.Codes.CatCode
  getCategory p = use $ typed @HexState % to (stateLocalCategory p)

  getSymbol :: H.Sym.ControlSymbol -> m (Maybe H.Sym.Tok.ResolvedToken)
  getSymbol p = use $ typed @HexState % to (stateLocalResolvedToken p)

  setSymbol v scope tgt = case scope of
    H.Sym.Tok.Local -> assign' (typed @HexState % stateLocalMostScopeLens % H.St.Scope.scopeSymbolLens v) (Just tgt)
    H.Sym.Tok.Global -> panic "Not implemented: assign global symbol"


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

  loadFont :: H.Syn.HexFilePath -> H.Syn.FontSpecification 'H.Syn.Evaluated -> m H.Inter.B.Box.FontDefinition
  loadFont path spec = do
    let filePath = path ^. typed @FilePath
    fontInfo <- readFontInfo filePath
    case spec of
      H.Syn.NaturalFont -> pure ()
      H.Syn.FontAt _ -> panic "not implemented: font-at"
      H.Syn.FontScaled _ -> panic "not implemented: font-scaled"
    mayLastKey <- use $ typed @HexState % #fontInfos % to Map.lookupMax
    let newKey = case mayLastKey of
          Nothing -> H.Sym.Tok.FontNumber $ H.Q.HexInt 0
          Just (i, _) -> succ i
    assign' (typed @HexState % #fontInfos % at' newKey) (Just fontInfo)

    let fontName = Tx.pack $ FilePath.takeBaseName filePath
    pure
      H.Inter.B.Box.FontDefinition
        { H.Inter.B.Box.fontDefChecksum = panic "Not implemented: fontDefChecksum",
          H.Inter.B.Box.fontDefDesignSize = panic "Not implemented: fontDefDesignSize",
          H.Inter.B.Box.fontDefDesignScale = panic "Not implemented: fontDefDesignScale",
          H.Inter.B.Box.fontNr = newKey,
          H.Inter.B.Box.fontPath = path,
          H.Inter.B.Box.fontName = fontName
        }

  selectFont :: H.Sym.Tok.FontNumber -> H.Sym.Tok.ScopeFlag -> m ()
  selectFont fNr scopeFlag = modifying' (typed @HexState) (selectFontNr fNr scopeFlag)

  setAfterAssignmentToken = assign' (typed @HexState % #afterAssignmentToken)

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
  hyphenChar <- getIntVariable (H.Syn.ParamVar H.Sym.Tok.DefaultHyphenChar)
  skewChar <- getIntVariable (H.Syn.ParamVar H.Sym.Tok.DefaultSkewChar)
  pure FontInfo {fontMetrics, hyphenChar, skewChar}
