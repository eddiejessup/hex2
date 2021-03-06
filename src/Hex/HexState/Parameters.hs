module Hex.HexState.Parameters where

import Data.Map.Strict qualified as Map
import Hex.Interpret.Evaluate.Evaluated qualified as H.Inter.Eval
import Hex.Symbol.Tokens qualified as H.Sym.Tok
import Protolude

newIntParameters :: Map H.Sym.Tok.IntParameter H.Inter.Eval.HexInt
newIntParameters =
  Map.fromList
    [ (H.Sym.Tok.Tolerance, 10000),
      (H.Sym.Tok.EscapeChar, 92), -- '\'
      (H.Sym.Tok.EndLineChar, 13), -- '\r'
      (H.Sym.Tok.MaxDeadCycles, 25),
      (H.Sym.Tok.HangAfter, 1),
      (H.Sym.Tok.Mag, 1000),
      (H.Sym.Tok.Time, 1),
      (H.Sym.Tok.Day, 1),
      (H.Sym.Tok.Month, 1),
      (H.Sym.Tok.Year, 1970)
    ]

newLengthParameters :: Map H.Sym.Tok.LengthParameter H.Inter.Eval.Length
newLengthParameters = mempty

newGlueParameters :: Map H.Sym.Tok.GlueParameter (H.Inter.Eval.Glue H.Inter.Eval.Length)
newGlueParameters = mempty

newMathGlueParameters :: Map H.Sym.Tok.MathGlueParameter (H.Inter.Eval.Glue H.Inter.Eval.MathLength)
newMathGlueParameters = mempty

newTokenListParameters :: Map H.Sym.Tok.TokenListParameter H.Sym.Tok.BalancedText
newTokenListParameters = mempty

newSpecialIntParameters :: Map H.Sym.Tok.SpecialIntParameter H.Inter.Eval.HexInt
newSpecialIntParameters = mempty

newSpecialLengthParameters :: Map H.Sym.Tok.SpecialLengthParameter H.Inter.Eval.Length
newSpecialLengthParameters =
  Map.fromList
    [ (H.Sym.Tok.PrevDepth, - H.Inter.Eval.oneKPt)
    ]

-- usableIntParameters :: Map IntParameter Int
-- usableIntParameters =
--     Map.union newIntParameters $ Map.fromList
--         [ (Tolerance, 500)
--         , (LinePenalty, 10)
--         , (Mag, 1000)
--         ]

-- usableLengthParameters :: Map H.Sym.Tok.LengthParameter Length
-- usableLengthParameters =
--     Map.union newLengthParameters $ Map.fromList
--         [ (HSize, 30750000)
--         , (VSize, 37500000)
--         , (ParIndent, toScaledPointApprox (20 :: Int) Point)
--         ]

-- usableGlueParameters :: Map H.Sym.Tok.GlueParameter (BL.G.Glue Length)
-- usableGlueParameters =
--     Map.union newGlueParameters $ Map.fromList
--         [ (BaselineSkip , BL.G.fixedGlue $ toScaledPointApprox (12 :: Int) Point )
--         , (LineSkip, BL.G.fixedGlue $ toScaledPointApprox (1 :: Int) Point)
--         ]
