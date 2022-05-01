module Hex.Common.HexState.Impl.Parameters where

import Data.Map.Strict qualified as Map
import Hex.Common.Quantity qualified as H.Q
import Hexlude
import qualified Hex.Common.HexState.Interface.Resolve.PrimitiveToken as PT
import qualified Hex.Common.HexState.Interface.Resolve.SyntaxToken as PT.Syn

newIntParameters :: Map PT.IntParameter H.Q.HexInt
newIntParameters =
  Map.fromList
    [ (PT.Tolerance, H.Q.HexInt 10000),
      (PT.EscapeChar, H.Q.HexInt 92), -- '\'
      (PT.EndLineChar, H.Q.HexInt 13), -- '\r'
      (PT.MaxDeadCycles, H.Q.HexInt 25),
      (PT.HangAfter, H.Q.HexInt 1),
      (PT.Mag, H.Q.HexInt 1000),
      (PT.Time, H.Q.HexInt 1),
      (PT.Day, H.Q.HexInt 1),
      (PT.Month, H.Q.HexInt 1),
      (PT.Year, H.Q.HexInt 1970)
    ]

newLengthParameters :: Map PT.LengthParameter H.Q.Length
newLengthParameters = mempty

newGlueParameters :: Map PT.GlueParameter H.Q.Glue
newGlueParameters = mempty

newMathGlueParameters :: Map PT.MathGlueParameter H.Q.MathGlue
newMathGlueParameters = mempty

newTokenListParameters :: Map PT.TokenListParameter PT.Syn.InhibitedBalancedText
newTokenListParameters = mempty

newSpecialIntParameters :: Map PT.SpecialIntParameter H.Q.HexInt
newSpecialIntParameters = mempty

newSpecialLengthParameters :: Map PT.SpecialLengthParameter H.Q.Length
newSpecialLengthParameters =
  Map.fromList
    [ (PT.PrevDepth, invert H.Q.oneKPt)
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
