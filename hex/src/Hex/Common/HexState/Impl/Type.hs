module Hex.Common.HexState.Impl.Type where

import Hex.Common.HexState.Impl.GroupScopes (GroupScopes, newGroupScopes)
import Hex.Common.HexState.Impl.Parameters qualified as Param
import Hex.Common.HexState.Interface.Resolve.PrimitiveToken qualified as PT
import Hex.Common.Quantity qualified as Q
import Hex.Common.TFM.Types qualified as H.TFM
import Hex.Stage.Lex.Interface.Extract qualified as Lex
import Hexlude

data HexState = HexState
  { fontInfos :: Map PT.FontNumber FontInfo,
    -- searchDirectories :: [Path Abs Dir],
    -- File streams.
    -- logStream :: Handle,
    -- outFileStreams :: Map FourBitInt Handle,
    -- internalLoggerSet :: Log.LoggerSet,
    -- / File streams.
    -- Global parameters.
    specialInts :: Map PT.SpecialIntParameter Q.HexInt,
    specialLengths :: Map PT.SpecialLengthParameter Q.Length,
    afterAssignmentToken :: Maybe Lex.LexToken,
    -- Scopes and groups.
    groupScopes :: GroupScopes,
    -- Just for parsing support help.
    lastFetchedLexTok :: Maybe Lex.LexToken
  }
  deriving stock (Generic)

newHexState :: HexState
newHexState =
  HexState
    { fontInfos = mempty,
      specialInts = Param.newSpecialIntParameters,
      specialLengths = Param.newSpecialLengthParameters,
      -- , logStream = logHandle
      -- , outFileStreams = mempty
      afterAssignmentToken = Nothing,
      groupScopes = newGroupScopes,
      -- , internalLoggerSet
      lastFetchedLexTok = Nothing
    }

data FontInfo = FontInfo {fontMetrics :: H.TFM.Font, hyphenChar :: Q.HexInt, skewChar :: Q.HexInt}
  deriving stock (Show, Generic)

stateSpecialLengthParamLens :: PT.SpecialLengthParameter -> Lens' HexState Q.Length
stateSpecialLengthParamLens p = #specialLengths % at' p % non Q.zeroLength

stateSpecialIntParamLens :: PT.SpecialIntParameter -> Lens' HexState Q.HexInt
stateSpecialIntParamLens p = #specialInts % at' p % non Q.zeroInt

stateFontInfoLens :: PT.FontNumber -> Lens' HexState (Maybe FontInfo)
stateFontInfoLens fNr = #fontInfos % at' fNr
