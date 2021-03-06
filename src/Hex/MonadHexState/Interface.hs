module Hex.MonadHexState.Interface where

import Hex.Symbol.Tokens qualified as H.Sym.Tok
import Protolude
import Hex.Interpret.Evaluate.Evaluated qualified as H.Inter.Eval
import Hex.Codes qualified as H.Codes
import Hex.Lex.Types qualified as H.Lex

class Monad m => MonadHexState m where
  getIntParameter :: H.Sym.Tok.IntParameter -> m H.Inter.Eval.HexInt

  getLengthParameter :: H.Sym.Tok.LengthParameter -> m H.Inter.Eval.Length

  getGlueParameter :: H.Sym.Tok.GlueParameter -> m (H.Inter.Eval.Glue H.Inter.Eval.Length)

  getSpecialLengthParameter :: H.Sym.Tok.SpecialLengthParameter -> m H.Inter.Eval.Length

  setSpecialLengthParameter :: H.Sym.Tok.SpecialLengthParameter -> H.Inter.Eval.Length -> m ()

  getCategory :: H.Codes.CharCode -> m H.Codes.CatCode

  resolveSymbol :: H.Lex.LexSymbol -> m (Maybe H.Sym.Tok.ResolvedToken)

-- Lifting.

instance MonadHexState m => MonadHexState (ExceptT e m) where
  getIntParameter :: H.Sym.Tok.IntParameter -> ExceptT e m H.Inter.Eval.HexInt
  getIntParameter p = lift $ getIntParameter p

  getLengthParameter :: H.Sym.Tok.LengthParameter -> ExceptT e m H.Inter.Eval.Length
  getLengthParameter p = lift $ getLengthParameter p

  getGlueParameter :: H.Sym.Tok.GlueParameter -> ExceptT e m (H.Inter.Eval.Glue H.Inter.Eval.Length)
  getGlueParameter p = lift $ getGlueParameter p

  getSpecialLengthParameter :: H.Sym.Tok.SpecialLengthParameter -> ExceptT e m H.Inter.Eval.Length
  getSpecialLengthParameter p = lift $ getSpecialLengthParameter p

  setSpecialLengthParameter :: H.Sym.Tok.SpecialLengthParameter -> H.Inter.Eval.Length -> ExceptT e m ()
  setSpecialLengthParameter p v = lift $ setSpecialLengthParameter p v

  getCategory :: H.Codes.CharCode -> ExceptT e m H.Codes.CatCode
  getCategory p = lift $ getCategory p

  resolveSymbol :: H.Lex.LexSymbol -> ExceptT e m (Maybe H.Sym.Tok.ResolvedToken)
  resolveSymbol a = lift $ resolveSymbol a
