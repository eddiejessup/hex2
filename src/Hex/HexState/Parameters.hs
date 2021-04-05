module Hex.HexState.Parameters where

import Data.Map.Strict qualified as Map
import Hex.Quantity qualified as H.Q
import Hex.Symbol.Tokens qualified as H.Sym.Tok
import Hexlude

newIntParameters :: Map H.Sym.Tok.IntParameter H.Q.HexInt
newIntParameters =
  Map.fromList
    [ (H.Sym.Tok.Tolerance, H.Q.HexInt 10000),
      (H.Sym.Tok.EscapeChar, H.Q.HexInt 92), -- '\'
      (H.Sym.Tok.EndLineChar, H.Q.HexInt 13), -- '\r'
      (H.Sym.Tok.MaxDeadCycles, H.Q.HexInt 25),
      (H.Sym.Tok.HangAfter, H.Q.HexInt 1),
      (H.Sym.Tok.Mag, H.Q.HexInt 1000),
      (H.Sym.Tok.Time, H.Q.HexInt 1),
      (H.Sym.Tok.Day, H.Q.HexInt 1),
      (H.Sym.Tok.Month, H.Q.HexInt 1),
      (H.Sym.Tok.Year, H.Q.HexInt 1970)
    ]

newLengthParameters :: Map H.Sym.Tok.LengthParameter H.Q.Length
newLengthParameters = mempty

newGlueParameters :: Map H.Sym.Tok.GlueParameter H.Q.Glue
newGlueParameters = mempty

newMathGlueParameters :: Map H.Sym.Tok.MathGlueParameter H.Q.MathGlue
newMathGlueParameters = mempty

newTokenListParameters :: Map H.Sym.Tok.TokenListParameter H.Sym.Tok.InhibitedBalancedText
newTokenListParameters = mempty

newSpecialIntParameters :: Map H.Sym.Tok.SpecialIntParameter H.Q.HexInt
newSpecialIntParameters = mempty

newSpecialLengthParameters :: Map H.Sym.Tok.SpecialLengthParameter H.Q.Length
newSpecialLengthParameters =
  Map.fromList
    [ (H.Sym.Tok.PrevDepth, invert H.Q.oneKPt)
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
