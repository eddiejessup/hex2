module Hex.Common.HexState.Impl.Defaults.Parameter where

import Data.Map.Strict qualified as Map
import Hex.Common.HexState.Interface.Parameter qualified as Param
import Hex.Common.HexState.Interface.TokenList qualified as TL
import Hex.Common.Quantity qualified as Q
import Hexlude
import qualified Data.Time as Time

-- INITEX sets almost all of the numeric registers and parameters equal to zero;
-- it makes all of the token registers and parameters empty; and it makes all of
-- the box registers void. But there are a few exceptions: \mag is set initially
-- to 1000, \tolerance to 10000, \maxdeadcycles to 25, \hangafter to 1,
-- \escapechar to ‘\\, and \endlinechar to ‘\^^M.

-- >>> timeStats
-- (65852,15,7,2022)
timeStats :: MonadIO m => m (Int, Time.DayOfMonth, Time.MonthOfYear, Time.Year)
timeStats = do
  zonedTime <- liftIO Time.getZonedTime
  let
    localTime = zonedTime.zonedTimeToLocalTime
    localTimeOfDay = localTime.localTimeOfDay

    localTimeSinceMidnight = Time.sinceMidnight localTimeOfDay
    picosecondsSinceMidnight = Time.diffTimeToPicoseconds localTimeSinceMidnight
    secondsSinceMidnight = fromIntegral @Integer @Int (picosecondsSinceMidnight `div` (10 ^ (12 :: Int)))

    localDay = localTime.localDay
    (yearInteger, monthOfYear, dayOfMonth) = Time.toGregorian localDay
  pure (secondsSinceMidnight, dayOfMonth, monthOfYear, yearInteger)

newEndLineChar :: Int
newEndLineChar = 13 -- '\r'

newIntParameters :: MonadIO m => m (Map Param.IntParameter Q.HexInt)
newIntParameters = do
  (secondsSinceMidnight, dayOfMonth, monthOfYear, yearInteger) <- timeStats
  let yearInt = fromIntegral @Integer @Int yearInteger
  pure $ Map.fromList
    [ (Param.Tolerance, Q.HexInt 10000),
      (Param.EscapeChar, Q.HexInt 92), -- '\'
      (Param.EndLineChar, Q.HexInt newEndLineChar), -- We insert this at the end of each input line.
      (Param.MaxDeadCycles, Q.HexInt 25),
      (Param.HangAfter, Q.HexInt 1),
      (Param.Mag, Q.HexInt 1000),

      (Param.Time, Q.HexInt secondsSinceMidnight),
      (Param.Day, Q.HexInt dayOfMonth),
      (Param.Month, Q.HexInt monthOfYear),
      (Param.Year, Q.HexInt yearInt),

      (Param.PreTolerance, Q.zeroInt),
      (Param.HBadness, Q.zeroInt),
      (Param.VBadness, Q.zeroInt),
      (Param.LinePenalty, Q.zeroInt),
      (Param.HyphenPenalty, Q.zeroInt),
      (Param.ExHyphenPenalty, Q.zeroInt),
      (Param.BinOpPenalty, Q.zeroInt),
      (Param.RelPenalty, Q.zeroInt),
      (Param.ClubPenalty, Q.zeroInt),
      (Param.WidowPenalty, Q.zeroInt),
      (Param.DisplayWidowPenalty, Q.zeroInt),
      (Param.BrokenPenalty, Q.zeroInt),
      (Param.PreDisplayPenalty, Q.zeroInt),
      (Param.PostDisplayPenalty, Q.zeroInt),
      (Param.InterlinePenalty, Q.zeroInt),
      (Param.FloatingPenalty, Q.zeroInt),
      (Param.OutputPenalty, Q.zeroInt),
      (Param.DoubleHyphenDemerits, Q.zeroInt),
      (Param.FinalHyphenDemerits, Q.zeroInt),
      (Param.AdjDemerits, Q.zeroInt),
      (Param.Looseness, Q.zeroInt),
      (Param.Pausing, Q.zeroInt),
      (Param.HoldingInserts, Q.zeroInt),
      (Param.TracingOnline, Q.zeroInt),
      (Param.TracingMacros, Q.zeroInt),
      (Param.TracingStats, Q.zeroInt),
      (Param.TracingParagraphs, Q.zeroInt),
      (Param.TracingPages, Q.zeroInt),
      (Param.TracingOutput, Q.zeroInt),
      (Param.TracingLostChars, Q.zeroInt),
      (Param.TracingCommands, Q.zeroInt),
      (Param.TracingRestores, Q.zeroInt),
      (Param.Language, Q.zeroInt),
      (Param.UCHyph, Q.zeroInt),
      (Param.LeftHyphenMin, Q.zeroInt),
      (Param.RightHyphenMin, Q.zeroInt),
      (Param.GlobalDefs, Q.zeroInt),
      (Param.DefaultHyphenChar, Q.zeroInt),
      (Param.DefaultSkewChar, Q.zeroInt),
      (Param.NewLineChar, Q.zeroInt),
      (Param.Fam, Q.zeroInt),
      (Param.DelimiterFactor, Q.zeroInt),
      (Param.ShowBoxBreadth, Q.zeroInt),
      (Param.ShowBoxDepth, Q.zeroInt),
      (Param.ErrorContextLines, Q.zeroInt)
    ]

newLengthParameters :: Map Param.LengthParameter Q.Length
newLengthParameters = Map.fromList [
    (Param.HFuzz, Q.zeroLength),
    (Param.VFuzz, Q.zeroLength),
    (Param.OverfullRule, Q.zeroLength),
    (Param.EmergencyStretch, Q.zeroLength),
    (Param.HSize, Q.zeroLength),
    (Param.VSize, Q.zeroLength),
    (Param.MaxDepth, Q.zeroLength),
    (Param.SplitMaxDepth, Q.zeroLength),
    (Param.BoxMaxDepth, Q.zeroLength),
    (Param.LineSkipLimit, Q.zeroLength),
    (Param.DelimiterShortfall, Q.zeroLength),
    (Param.NullDelimiterSpace, Q.zeroLength),
    (Param.ScriptSpace, Q.zeroLength),
    (Param.MathSurround, Q.zeroLength),
    (Param.PreDisplaySize, Q.zeroLength),
    (Param.DisplayWidth, Q.zeroLength),
    (Param.DisplayIndent, Q.zeroLength),
    (Param.ParIndent, Q.zeroLength),
    (Param.HangIndent, Q.zeroLength),
    (Param.HOffset, Q.zeroLength),
    (Param.VOffset, Q.zeroLength)
  ]

newGlueParameters :: Map Param.GlueParameter Q.Glue
newGlueParameters = Map.fromList [
    (Param.BaselineSkip, Q.zeroGlue),
    (Param.LineSkip, Q.zeroGlue),
    (Param.ParSkip, Q.zeroGlue),
    (Param.AboveDisplaySkip, Q.zeroGlue),
    (Param.AboveDisplayShortSkip, Q.zeroGlue),
    (Param.BelowDisplaySkip, Q.zeroGlue),
    (Param.BelowDisplayShortSkip, Q.zeroGlue),
    (Param.LeftSkip, Q.zeroGlue),
    (Param.RightSkip, Q.zeroGlue),
    (Param.TopSkip, Q.zeroGlue),
    (Param.SplitTopSkip, Q.zeroGlue),
    (Param.TabSkip, Q.zeroGlue),
    (Param.SpaceSkip, Q.zeroGlue),
    (Param.XSpaceSkip, Q.zeroGlue),
    (Param.ParFillSkip, Q.zeroGlue)
  ]

newMathGlueParameters :: Map Param.MathGlueParameter Q.MathGlue
newMathGlueParameters = Map.fromList [
    (Param.ThinMuSkip, Q.zeroMathGlue),
    (Param.MedMuSkip, Q.zeroMathGlue),
    (Param.ThickMuSkip, Q.zeroMathGlue)
  ]

newTokenListParameters :: Map Param.TokenListParameter TL.BalancedText
newTokenListParameters = Map.fromList [
    (Param.Output, TL.emptyBalancedText),
    (Param.EveryPar, TL.emptyBalancedText),
    (Param.EveryMath, TL.emptyBalancedText),
    (Param.EveryDisplay, TL.emptyBalancedText),
    (Param.EveryHBox, TL.emptyBalancedText),
    (Param.EveryVBox, TL.emptyBalancedText),
    (Param.EveryJob, TL.emptyBalancedText),
    (Param.EveryCR, TL.emptyBalancedText),
    (Param.ErrHelp, TL.emptyBalancedText)
  ]

newSpecialIntParameters :: Map Param.SpecialIntParameter Q.HexInt
newSpecialIntParameters = Map.fromList [
    (Param.SpaceFactor, Q.HexInt 1000),
    (Param.PrevGraf, Q.zeroInt),
    (Param.DeadCycles, Q.zeroInt),
    (Param.InsertPenalties, Q.zeroInt)
  ]

newSpecialLengthParameters :: Map Param.SpecialLengthParameter Q.Length
newSpecialLengthParameters =
  Map.fromList
    [ (Param.PrevDepth, invert Q.oneKPt),
      (Param.PageGoal, Q.zeroLength),
      (Param.PageTotal, Q.zeroLength),
      (Param.PageStretch, Q.zeroLength),
      (Param.PageFilStretch, Q.zeroLength),
      (Param.PageFillStretch, Q.zeroLength),
      (Param.PageFilllStretch, Q.zeroLength),
      (Param.PageShrink, Q.zeroLength),
      (Param.PageDepth, Q.zeroLength)
    ]
