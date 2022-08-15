module Hex.Stage.Build.Horizontal.Paragraph.Break.MultiPass where

import Formatting qualified as F
import Hex.Capability.Log.Interface qualified as Log
import Hex.Common.Quantity qualified as Q
import Hex.Stage.Build.Horizontal.Paragraph.Break.Optimal qualified as Optimal
import Hex.Stage.Build.ListElem (HList)
import Hex.Stage.Build.ListElem qualified as ListElem
import Hexlude

hyphenateHList ::
  ListElem.HList ->
  ListElem.HList
hyphenateHList = notImplemented "hListWithDiscretionaries"

breakHListMultiPass ::
  Log.HexLog :> es =>
  Q.Length -> -- HSize
  ListElem.Penalty -> -- hyphenpenalty
  ListElem.Penalty -> -- exhyphenpenalty
  Q.HexInt -> -- PreTolerance
  Q.HexInt -> -- Tolerance
  Q.HexInt -> -- LinePenalty
  Q.Length -> -- Emergency stretch
  ListElem.HList ->
  Eff es (Seq HList)
breakHListMultiPass desiredWidth hyphenPenalty exHyphenPenalty preTolerance tolerance linePenalty emergencyStretch rawHList = do
  passOneResult <-
    if (preTolerance >= Q.zeroInt)
      then breakIt preTolerance rawHList
      else pure Nothing
  case passOneResult of
    Just hLists -> pure hLists
    Nothing -> do
      Log.infoLog $
        F.sformat
          ( "Failed to break with pretolerance: "
              |%| Q.fmtHexIntSimple
              |%| ", hyphenating and retrying with tolerance: "
              |%| Q.fmtHexIntSimple
          )
          preTolerance
          tolerance
      let hListWithDiscretionaries = hyphenateHList rawHList
      breakIt tolerance hListWithDiscretionaries >>= \case
        Just hLists -> pure hLists
        Nothing ->
          if emergencyStretch > Q.zeroLength
            then notImplemented "Emergency stretch"
            else notImplemented "breakHListMultiPass: Hyphenated pass fails"
  where
    breakIt tol hList =
      Optimal.breakHListOptimally desiredWidth hyphenPenalty exHyphenPenalty tol linePenalty hList
