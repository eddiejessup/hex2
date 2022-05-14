module Hex.Run.Usability where

import Data.Map qualified as Map
import Hex.Common.HexState.Interface.Parameter qualified as Param
import Hex.Common.Quantity qualified as Q
import Hexlude

usableIntParameters :: Map Param.IntParameter Q.HexInt
usableIntParameters =
  Map.union Param.newIntParameters $
    Map.fromList
      [ (Param.Tolerance, Q.HexInt 500),
        (Param.LinePenalty, Q.HexInt 10),
        (Param.Mag, Q.HexInt 1000)
      ]

usableLengthParameters :: Map Param.LengthParameter Q.Length
usableLengthParameters =
  Map.union Param.newLengthParameters $
    Map.fromList
      -- A4 page dimensions: 210 x 297 mm
      -- Typical A4 margins: 1 inch top and bottom, 1.25 inches each side.
      [ (Param.HSize, Q.mm 210 ~~ Q.inch 1 ~~ Q.inch 1),
        (Param.VSize, Q.pt 297 ~~ Q.inch 1.25 ~~ Q.inch 1.25),
        (Param.ParIndent, (Q.pt 20))
      ]

usableGlueParameters :: Map Param.GlueParameter Q.Glue
usableGlueParameters =
  Map.union Param.newGlueParameters $
    Map.fromList
      [ (Param.BaselineSkip, Q.fixedGlue $ Q.pt 12),
        (Param.LineSkip, Q.fixedGlue $ Q.pt 1)
      ]
