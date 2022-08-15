module Hex.Stage.Build.Horizontal.Paragraph.Break.MultiPass where

import Formatting qualified as F
import Hex.Capability.Log.Interface qualified as Log
import Hex.Common.Quantity qualified as Q
import Hex.Stage.Build.AnyDirection.Breaking.Badness qualified as Bad
import Hex.Stage.Build.Horizontal.Paragraph.Break.Common (LineBreakingEnv (..), mkLineBreakingEnv)
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
  Q.HexInt -> -- hyphenpenalty
  Q.HexInt -> -- exhyphenpenalty
  Q.HexInt -> -- PreTolerance
  Q.HexInt -> -- Tolerance
  Q.HexInt -> -- LinePenalty
  Q.Length -> -- Emergency stretch
  Q.Glue -> -- LeftSkip
  Q.Glue -> -- RightSkip
  ListElem.HList ->
  Eff es (Seq HList)
breakHListMultiPass
  hSize
  hyphenPenalty
  exHyphenPenalty
  preTolerance
  tolerance
  linePenalty
  emergencyStretch
  leftSkip
  rightSkip
  rawHList = do
    let breakingEnv =
          mkLineBreakingEnv
            hSize
            emergencyStretch
            hyphenPenalty
            exHyphenPenalty
            preTolerance
            linePenalty
            leftSkip
            rightSkip

    passOneResult <- runReader breakingEnv $ do
      activeTol <- know @LineBreakingEnv #tolerance
      if (activeTol >= Bad.zeroFiniteBadness)
        then breakIt rawHList
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
            secondPassEnv =
              mkLineBreakingEnv
                hSize
                emergencyStretch
                hyphenPenalty
                exHyphenPenalty
                tolerance
                linePenalty
                leftSkip
                rightSkip

        runReader secondPassEnv (breakIt hListWithDiscretionaries) >>= \case
          Just hLists -> pure hLists
          Nothing ->
            if emergencyStretch > Q.zeroLength
              then notImplemented "Emergency stretch"
              else notImplemented "breakHListMultiPass: Hyphenated pass fails"
    where
      breakIt hList =
        Optimal.breakHListOptimally hList
