module Hex.MonadHexState.Interface where

import Hex.Codes qualified as H.Codes
import Hex.Quantity qualified as H.Q
import Hex.Symbol.Tokens qualified as H.Sym.Tok
import Hex.Symbol.Types qualified as H.Sym
import Hexlude

newtype FontNumber = FontNumber H.Q.HexInt
  deriving stock (Show)
  deriving newtype (Eq, Ord, Enum)

class Monad m => MonadHexState m where
  getIntParameter :: H.Sym.Tok.IntParameter -> m H.Q.HexInt

  getLengthParameter :: H.Sym.Tok.LengthParameter -> m H.Q.Length

  getGlueParameter :: H.Sym.Tok.GlueParameter -> m H.Q.Glue

  getSpecialLengthParameter :: H.Sym.Tok.SpecialLengthParameter -> m H.Q.Length

  setSpecialLengthParameter :: H.Sym.Tok.SpecialLengthParameter -> H.Q.Length -> m ()

  getCategory :: H.Codes.CharCode -> m H.Codes.CatCode

  resolveSymbol :: H.Sym.ControlSymbol -> m (Maybe H.Sym.Tok.ResolvedToken)

  loadFont :: FilePath -> m (FontNumber, Text)

  selectFont :: FontNumber -> H.Sym.Tok.ScopeFlag -> m ()

  currentFontCharacter :: H.Codes.CharCode -> m (Maybe (H.Q.Length, H.Q.Length, H.Q.Length, H.Q.Length))

  currentFontSpaceGlue :: m (Maybe H.Q.Glue)

-- Lifting.
instance MonadHexState m => MonadHexState (ExceptT e m) where
  getIntParameter = lift . getIntParameter

  getLengthParameter = lift . getLengthParameter

  getGlueParameter = lift . getGlueParameter

  getSpecialLengthParameter = lift . getSpecialLengthParameter

  setSpecialLengthParameter p v = lift $ setSpecialLengthParameter p v

  getCategory p = lift $ getCategory p

  resolveSymbol a = lift $ resolveSymbol a

  loadFont = lift . loadFont

  selectFont a b = lift $ selectFont a b

  currentFontCharacter = lift . currentFontCharacter

  currentFontSpaceGlue = lift currentFontSpaceGlue
