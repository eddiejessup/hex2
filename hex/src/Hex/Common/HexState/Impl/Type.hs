module Hex.Common.HexState.Impl.Type where

import Formatting qualified as F
import Hex.Common.HexState.Impl.Font qualified as HSt.Font
import Hex.Common.HexState.Impl.Scoped.GroupScopes (GroupScopes, fmtGroupScopes, newGroupScopes)
import Hex.Common.HexState.Interface.Resolve.PrimitiveToken qualified as PT
import Hex.Common.Parameters qualified as Param
import Hex.Common.Quantity qualified as Q
import Hex.Stage.Lex.Interface.Extract qualified as Lex
import Hexlude

data HexState = HexState
  { fontInfos :: Map PT.FontNumber HSt.Font.FontInfo,
    outFileStreams :: Map Q.FourBitInt Handle,
    -- Global parameters.
    specialInts :: Map PT.SpecialIntParameter Q.HexInt,
    specialLengths :: Map PT.SpecialLengthParameter Q.Length,
    afterAssignmentToken :: Maybe Lex.LexToken,
    groupScopes :: GroupScopes
  }
  deriving stock (Generic)

fmtHexState :: Fmt HexState
fmtHexState =
  mconcat
    [ fmtMapWithHeading "FontInfos" (.fontInfos) PT.fmtFontNumber HSt.Font.fmtFontInfo,
      fmtMapWithHeading "Special integer parameters" (.specialInts) PT.fmtSpecialIntParameter Q.fmtHexInt,
      fmtMapWithHeading "Special length parameters" (.specialLengths) PT.fmtSpecialLengthParameter Q.fmtLengthWithUnit,
      F.prefixed "After-assignnment token: " $ F.accessed (.afterAssignmentToken) (F.maybed "None" Lex.fmtLexToken) |%| "\n",
      F.accessed (.groupScopes) fmtGroupScopes
    ]

newHexState :: HexState
newHexState =
  HexState
    { fontInfos = mempty,
      specialInts = Param.newSpecialIntParameters,
      specialLengths = Param.newSpecialLengthParameters,
      outFileStreams = mempty,
      afterAssignmentToken = Nothing,
      groupScopes = newGroupScopes
    }

stateSpecialLengthParamLens :: PT.SpecialLengthParameter -> Lens' HexState Q.Length
stateSpecialLengthParamLens p = #specialLengths % at' p % non Q.zeroLength

stateSpecialIntParamLens :: PT.SpecialIntParameter -> Lens' HexState Q.HexInt
stateSpecialIntParamLens p = #specialInts % at' p % non Q.zeroInt

stateFontInfoLens :: PT.FontNumber -> Lens' HexState (Maybe HSt.Font.FontInfo)
stateFontInfoLens fNr = #fontInfos % at' fNr

fmtSpecialInts :: Fmt (Map PT.SpecialIntParameter Q.HexInt)
fmtSpecialInts = F.shown

fmtSpecialLengths :: Fmt (Map PT.SpecialLengthParameter Q.Length)
fmtSpecialLengths = F.shown
