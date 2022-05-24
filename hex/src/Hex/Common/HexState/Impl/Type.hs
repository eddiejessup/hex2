module Hex.Common.HexState.Impl.Type where

import Data.Map.Strict qualified as Map
import Formatting qualified as F
import Hex.Common.HexState.Impl.Font qualified as HSt.Font
import Hex.Common.HexState.Impl.Scoped.Font qualified as Sc.Font
import Hex.Common.HexState.Impl.Scoped.GroupScopes (GroupScopes, fmtGroupScopes, newGroupScopes)
import Hex.Common.HexState.Impl.Scoped.Scope (nullFontNumber)
import Hex.Common.HexState.Interface.Font qualified as Font
import Hex.Common.HexState.Interface.Parameter qualified as Param
import Hex.Common.Quantity qualified as Q
import Hex.Stage.Lex.Interface.Extract qualified as Lex
import Hexlude

data HexState = HexState
  { fontInfos :: Map Font.FontNumber HSt.Font.FontInfo,
    outFileStreams :: Map Q.FourBitInt Handle,
    -- Global parameters.
    specialInts :: Map Param.SpecialIntParameter Q.HexInt,
    specialLengths :: Map Param.SpecialLengthParameter Q.Length,
    afterAssignmentToken :: Maybe Lex.LexToken,
    groupScopes :: GroupScopes
  }
  deriving stock (Generic)

fmtHexState :: Fmt HexState
fmtHexState =
  mconcat
    [ fmtMapWithHeading "FontInfos" (.fontInfos) Font.fmtFontNumber HSt.Font.fmtFontInfo,
      fmtMapWithHeading "Special integer parameters" (.specialInts) Param.fmtSpecialIntParameter Q.fmtHexInt,
      fmtMapWithHeading "Special length parameters" (.specialLengths) Param.fmtSpecialLengthParameter Q.fmtLengthWithUnit,
      F.prefixed "After-assignnment token: " $ F.accessed (.afterAssignmentToken) (F.maybed "None" Lex.fmtLexToken) |%| "\n",
      F.accessed (.groupScopes) fmtGroupScopes
    ]

newHexState :: HexState
newHexState =
  HexState
    { fontInfos = newFontInfos,
      specialInts = Param.newSpecialIntParameters,
      specialLengths = Param.newSpecialLengthParameters,
      outFileStreams = mempty,
      afterAssignmentToken = Nothing,
      groupScopes = newGroupScopes
    }
  where
    newFontInfos = Map.fromList [(nullFontNumber, HSt.Font.nullFontInfo)]

stateSpecialLengthParamLens :: Param.SpecialLengthParameter -> Lens' HexState Q.Length
stateSpecialLengthParamLens p = #specialLengths % at' p % non Q.zeroLength

stateSpecialIntParamLens :: Param.SpecialIntParameter -> Lens' HexState Q.HexInt
stateSpecialIntParamLens p = #specialInts % at' p % non Q.zeroInt

stateFontInfoLens :: Font.FontNumber -> Lens' HexState (Maybe HSt.Font.FontInfo)
stateFontInfoLens fNr = #fontInfos % at' fNr

fmtSpecialInts :: Fmt (Map Param.SpecialIntParameter Q.HexInt)
fmtSpecialInts = F.shown

fmtSpecialLengths :: Fmt (Map Param.SpecialLengthParameter Q.Length)
fmtSpecialLengths = F.shown

getGroupScopesProperty ::
  (MonadState st m, HasType HexState st) => (GroupScopes -> a) -> m a
getGroupScopesProperty groupScopesGetter =
  use $ typed @HexState % #groupScopes % to groupScopesGetter

currentFontInfoImpl ::
  ( MonadState st m,
    HasType HexState st
  ) =>
  m (Maybe HSt.Font.FontInfo)
currentFontInfoImpl = do
  fNr <- currentFontNumberImpl
  use (typed @HexState % stateFontInfoLens fNr)

currentFontNumberImpl ::
  ( MonadState st m,
    HasType HexState st
  ) =>
  m Font.FontNumber
currentFontNumberImpl =
  getGroupScopesProperty Sc.Font.localCurrentFontNr
