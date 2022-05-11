module Hex.Run.Usability where

import Data.Map qualified as Map
import Hex.Common.HexState.Interface.Resolve.PrimitiveToken qualified as PT
import Hex.Common.Parameters qualified as Param
import Hex.Common.Quantity qualified as Q
import Hexlude

usableIntParameters :: Map PT.IntParameter Q.HexInt
usableIntParameters =
  Map.union Param.newIntParameters $
    Map.fromList
      [ (PT.Tolerance, Q.HexInt 500),
        (PT.LinePenalty, Q.HexInt 10),
        (PT.Mag, Q.HexInt 1000)
      ]

usableLengthParameters :: Map PT.LengthParameter Q.Length
usableLengthParameters =
  Map.union Param.newLengthParameters $
    Map.fromList
      -- A4 page dimensions: 210 x 297 mm
      -- Typical A4 margins: 1 inch top and bottom, 1.25 inches each side.
      [ (PT.HSize, Q.mm 210 ~~ Q.inch 1 ~~ Q.inch 1),
        (PT.VSize, Q.pt 297 ~~ Q.inch 1.25 ~~ Q.inch 1.25),
        (PT.ParIndent, (Q.pt 20))
      ]

usableGlueParameters :: Map PT.GlueParameter Q.Glue
usableGlueParameters =
  Map.union Param.newGlueParameters $
    Map.fromList
      [ (PT.BaselineSkip, Q.fixedGlue $ Q.pt 12),
        (PT.LineSkip, Q.fixedGlue $ Q.pt 1)
      ]
