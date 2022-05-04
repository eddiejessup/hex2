module Hex.Common.HexState.Impl.Parameters where

import Data.Map.Strict qualified as Map
import Hex.Common.HexState.Interface.Resolve.PrimitiveToken qualified as PT
import Hex.Common.HexState.Interface.Resolve.SyntaxToken qualified as ST
import Hex.Common.Quantity qualified as Q
import Hexlude

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

newTokenListParameters :: Map PT.TokenListParameter ST.InhibitedBalancedText
newTokenListParameters = mempty

newSpecialIntParameters :: Map PT.SpecialIntParameter Q.HexInt
newSpecialIntParameters = mempty

newSpecialLengthParameters :: Map PT.SpecialLengthParameter Q.Length
newSpecialLengthParameters =
  Map.fromList
    [ (PT.PrevDepth, invert Q.oneKPt)
    ]
