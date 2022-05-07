module Hex.Common.HexState.Impl.Type where

import Hex.Common.HexState.Impl.Font qualified as HSt.Font
import Hex.Common.HexState.Impl.Scoped.GroupScopes (GroupScopes, newGroupScopes)
import Hex.Common.HexState.Interface.Resolve.PrimitiveToken qualified as PT
import Hex.Common.Parameters qualified as Param
import Hex.Common.Quantity qualified as Q
import Hex.Stage.Lex.Interface.Extract qualified as Lex
import Hexlude

data HexState = HexState
  { fontInfos :: Map PT.FontNumber HSt.Font.FontInfo,
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

-- fmtHexState :: Fmt HexState a
-- fmtHexState =

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

stateSpecialLengthParamLens :: PT.SpecialLengthParameter -> Lens' HexState Q.Length
stateSpecialLengthParamLens p = #specialLengths % at' p % non Q.zeroLength

stateSpecialIntParamLens :: PT.SpecialIntParameter -> Lens' HexState Q.HexInt
stateSpecialIntParamLens p = #specialInts % at' p % non Q.zeroInt

stateFontInfoLens :: PT.FontNumber -> Lens' HexState (Maybe HSt.Font.FontInfo)
stateFontInfoLens fNr = #fontInfos % at' fNr
