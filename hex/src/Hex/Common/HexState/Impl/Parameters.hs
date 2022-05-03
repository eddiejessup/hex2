module Hex.Common.HexState.Impl.Parameters where

import Data.Map.Strict qualified as Map
import Hex.Common.Quantity qualified as Q
import Hexlude
import qualified Hex.Common.HexState.Interface.Resolve.PrimitiveToken as PT
import qualified Hex.Common.HexState.Interface.Resolve.SyntaxToken as PT.Syn

newIntParameters :: Map PT.IntParameter Q.HexInt
newIntParameters =
  Map.fromList
    [ (PT.Tolerance, Q.HexInt 10000),
      (PT.EscapeChar, Q.HexInt 92), -- '\'
      (PT.EndLineChar, Q.HexInt 13), -- '\r'
      (PT.MaxDeadCycles, Q.HexInt 25),
      (PT.HangAfter, Q.HexInt 1),
      (PT.Mag, Q.HexInt 1000),
      (PT.Time, Q.HexInt 1),
      (PT.Day, Q.HexInt 1),
      (PT.Month, Q.HexInt 1),
      (PT.Year, Q.HexInt 1970)
    ]

newLengthParameters :: Map PT.LengthParameter Q.Length
newLengthParameters = mempty

newGlueParameters :: Map PT.GlueParameter Q.Glue
newGlueParameters = mempty

newMathGlueParameters :: Map PT.MathGlueParameter Q.MathGlue
newMathGlueParameters = mempty

newTokenListParameters :: Map PT.TokenListParameter PT.Syn.InhibitedBalancedText
newTokenListParameters = mempty

newSpecialIntParameters :: Map PT.SpecialIntParameter Q.HexInt
newSpecialIntParameters = mempty

newSpecialLengthParameters :: Map PT.SpecialLengthParameter Q.Length
newSpecialLengthParameters =
  Map.fromList
    [ (PT.PrevDepth, invert Q.oneKPt)
    ]

-- usableIntParameters :: Map IntParameter Int
-- usableIntParameters =
--     Map.union newIntParameters $ Map.fromList
--         [ (Tolerance, 500)
--         , (LinePenalty, 10)
--         , (Mag, 1000)
--         ]

-- usableLengthParameters :: Map PT.LengthParameter Length
-- usableLengthParameters =
--     Map.union newLengthParameters $ Map.fromList
--         [ (HSize, 30750000)
--         , (VSize, 37500000)
--         , (ParIndent, toScaledPointApprox (20 :: Int) Point)
--         ]

-- usableGlueParameters :: Map PT.GlueParameter (BL.G.Glue Length)
-- usableGlueParameters =
--     Map.union newGlueParameters $ Map.fromList
--         [ (BaselineSkip , BL.G.fixedGlue $ toScaledPointApprox (12 :: Int) Point )
--         , (LineSkip, BL.G.fixedGlue $ toScaledPointApprox (1 :: Int) Point)
--         ]
