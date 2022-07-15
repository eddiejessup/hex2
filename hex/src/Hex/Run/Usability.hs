module Hex.Run.Usability where

import Data.Map qualified as Map
import Hex.Common.Codes qualified as Code
import Hex.Common.HexState.Impl.Defaults.Code qualified as Defaults
import Hex.Common.HexState.Impl.Defaults.Parameter qualified as Defaults
import Hex.Common.HexState.Interface.Parameter qualified as Param
import Hex.Common.Quantity qualified as Q
import Hexlude

usableIntParameters :: MonadIO m => m (Map Param.IntParameter Q.HexInt)
usableIntParameters = do
  newParams <- Defaults.newIntParameters
  pure $
    Map.union newParams $
      Map.fromList
        [ (Param.Tolerance, Q.HexInt 500),
          (Param.LinePenalty, Q.HexInt 10),
          (Param.Mag, Q.HexInt 1000)
        ]

usableLengthParameters :: Map Param.LengthParameter Q.Length
usableLengthParameters =
  Map.union Defaults.newLengthParameters $
    Map.fromList
      -- A4 page dimensions: 210 x 297 mm
      -- Typical A4 margins: 1 inch top and bottom, 1.25 inches each side.
      [ (Param.HSize, Q.mm 210 ~~ Q.inch 1 ~~ Q.inch 1),
        (Param.VSize, Q.pt 297 ~~ Q.inch 1.25 ~~ Q.inch 1.25),
        (Param.ParIndent, (Q.pt 20))
      ]

usableGlueParameters :: Map Param.GlueParameter Q.Glue
usableGlueParameters =
  Map.union Defaults.newGlueParameters $
    Map.fromList
      [ (Param.BaselineSkip, Q.fixedGlue $ Q.pt 12),
        (Param.LineSkip, Q.fixedGlue $ Q.pt 1)
      ]

-- Add useful extras beyond the technical defaults.
usableCatCodes :: Map Code.CharCode Code.CatCode
usableCatCodes = foldl' (\m (k, v) -> Map.insert k v m) Defaults.newCatCodes extras
  where
    extras =
      [ (Code.Chr_ '^', Code.CoreCatCode Code.Superscript),
        (Code.Chr_ '{', Code.CoreCatCode Code.BeginGroup),
        (Code.Chr_ '}', Code.CoreCatCode Code.EndGroup),
        (Code.Chr_ '#', Code.CoreCatCode Code.Parameter)
      ]
