{-# LANGUAGE UndecidableInstances #-}
{-# OPTIONS_GHC -Wno-orphans #-}

module Hex.HexState.Instances.MonadHexState where

import Hex.Interpret.Evaluate.Evaluated qualified as H.Inter.Eval
import Hex.HexState.Type
import Hex.MonadHexState.Interface
import Hex.Symbol.Tokens qualified as H.Sym.Tok
import Optics.Core ((%))
import Optics.Core qualified as O
import Optics.State qualified as O
import Protolude hiding ((%))
import Hex.Codes qualified as H.Codes
import Hex.Lex.Types qualified as H.Lex
import Data.Generics.Product qualified as G.P

instance (Monad m, MonadState st m, G.P.HasType HexState st) => MonadHexState m where
  getIntParameter :: H.Sym.Tok.IntParameter -> m H.Inter.Eval.HexInt
  getIntParameter p = O.use $ G.P.typed @HexState % O.to (stateLocalIntParam p)

  getLengthParameter :: H.Sym.Tok.LengthParameter -> m H.Inter.Eval.Length
  getLengthParameter p = O.use $ G.P.typed @HexState % O.to (stateLocalLengthParam p)

  getGlueParameter :: H.Sym.Tok.GlueParameter -> m (H.Inter.Eval.Glue H.Inter.Eval.Length)
  getGlueParameter p = O.use $ G.P.typed @HexState % O.to (stateLocalGlueParam p)

  getSpecialLengthParameter :: H.Sym.Tok.SpecialLengthParameter -> m H.Inter.Eval.Length
  getSpecialLengthParameter p = O.use $ G.P.typed @HexState % stateSpecialLengthParamLens p

  setSpecialLengthParameter :: H.Sym.Tok.SpecialLengthParameter -> H.Inter.Eval.Length -> m ()
  setSpecialLengthParameter p v = O.assign' (G.P.typed @HexState % stateSpecialLengthParamLens p) v

  getCategory :: H.Codes.CharCode -> m H.Codes.CatCode
  getCategory p = O.use $ G.P.typed @HexState % O.to (stateLocalCategory p)

  resolveSymbol :: H.Lex.LexSymbol -> m (Maybe H.Sym.Tok.ResolvedToken)
  resolveSymbol p = O.use $ G.P.typed @HexState % O.to (stateLocalResolvedToken p)
