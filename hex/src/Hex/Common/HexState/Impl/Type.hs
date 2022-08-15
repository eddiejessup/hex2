module Hex.Common.HexState.Impl.Type where

import Data.List.NonEmpty qualified as L.NE
import Data.Map.Strict qualified as Map
import Formatting qualified as F
import Hex.Common.HexState.Impl.Defaults.Parameter qualified as Defaults
import Hex.Common.HexState.Impl.Font qualified as HSt.Font
import Hex.Common.HexState.Impl.Scoped.Font qualified as Sc.Font
import Hex.Common.HexState.Impl.Scoped.GroupScopes (GroupScopes, fmtGroupScopes, newGroupScopes)
import Hex.Common.HexState.Impl.Scoped.Scope (nullFontNumber)
import Hex.Common.HexState.Interface.Mode qualified as HSt.Mode
import Hex.Common.HexState.Interface.Parameter qualified as Param
import Hex.Common.Quantity qualified as Q
import Hex.Common.Token.Lexed qualified as LT
import Hex.Stage.Render.Interface.DocInstruction qualified as DVI
import Hexlude

data HexState = HexState
  { fontInfos :: Map DVI.FontNumber HSt.Font.FontInfo,
    outFileStreams :: Map Q.FourBitInt Handle,
    -- GlobalScope parameters.
    specialInts :: Map Param.SpecialIntParameter Q.HexInt,
    specialLengths :: Map Param.SpecialLengthParameter Q.Length,
    afterAssignmentToken :: Maybe LT.LexToken,
    groupScopes :: GroupScopes,
    modeStack :: ModeStack
  }
  deriving stock (Generic)

data ModeStack
  = MainVMode
  | InNonMainVMode (NonEmpty HSt.Mode.NonMainVMode)

enterModeImpl :: HSt.Mode.NonMainVMode -> ModeStack -> ModeStack
enterModeImpl mode stack = InNonMainVMode $ case stack of
  MainVMode -> L.NE.singleton mode
  InNonMainVMode xs -> (L.NE.cons mode xs)

leaveModeImpl :: ModeStack -> Maybe ModeStack
leaveModeImpl (InNonMainVMode (_x :| xs)) =
  case L.NE.nonEmpty xs of
    Nothing -> Just MainVMode
    Just restNonMVModes -> Just $ InNonMainVMode restNonMVModes
leaveModeImpl MainVMode = Nothing

peekModeImpl :: ModeStack -> HSt.Mode.ModeWithVariant
peekModeImpl (InNonMainVMode (x :| _)) = HSt.Mode.asModeWithVariant x
peekModeImpl MainVMode = HSt.Mode.ModeWithVariant HSt.Mode.VerticalMode HSt.Mode.OuterModeVariant

fmtHexState :: Fmt HexState
fmtHexState =
  mconcat
    [ fmtMapWithHeading "FontInfos" (.fontInfos) DVI.fmtFontNumber HSt.Font.fmtFontInfo,
      fmtMapWithHeading "Special integer parameters" (.specialInts) Param.fmtSpecialIntParameter Q.fmtHexInt,
      fmtMapWithHeading "Special length parameters" (.specialLengths) Param.fmtSpecialLengthParameter Q.fmtLengthWithUnit,
      F.prefixed "After-assignnment token: " $ F.accessed (.afterAssignmentToken) (F.maybed "None" LT.fmtLexToken) |%| "\n",
      F.accessed (.groupScopes) fmtGroupScopes
    ]

newHexState :: MonadIO m => m HexState
newHexState = do
  groupScopes <- newGroupScopes
  pure
    HexState
      { fontInfos = newFontInfos,
        specialInts = Defaults.newSpecialIntParameters,
        specialLengths = Defaults.newSpecialLengthParameters,
        outFileStreams = mempty,
        afterAssignmentToken = Nothing,
        groupScopes,
        modeStack = MainVMode
      }
  where
    newFontInfos = Map.fromList [(nullFontNumber, HSt.Font.nullFontInfo)]

stateSpecialLengthParamLens :: Param.SpecialLengthParameter -> Lens' HexState Q.Length
stateSpecialLengthParamLens p = #specialLengths % at' p % non Q.zeroLength

stateSpecialIntParamLens :: Param.SpecialIntParameter -> Lens' HexState Q.HexInt
stateSpecialIntParamLens p = #specialInts % at' p % non Q.zeroInt

stateFontInfoLens :: DVI.FontNumber -> Lens' HexState HSt.Font.FontInfo
stateFontInfoLens fontNumber =
  lens getter setter
  where
    getter st =
      case Map.lookup fontNumber st.fontInfos of
        Nothing -> panic "Impossible!"
        Just v -> v

    setter st v =
      st {fontInfos = Map.insert fontNumber v st.fontInfos}

fmtSpecialInts :: Fmt (Map Param.SpecialIntParameter Q.HexInt)
fmtSpecialInts = F.shown

fmtSpecialLengths :: Fmt (Map Param.SpecialLengthParameter Q.Length)
fmtSpecialLengths = F.shown

getGroupScopesProperty ::
  State HexState :> es => (GroupScopes -> a) -> Eff es a
getGroupScopesProperty groupScopesGetter =
  use @HexState $ #groupScopes % to groupScopesGetter

currentFontInfoImpl ::
  State HexState :> es =>
  Eff es HSt.Font.FontInfo
currentFontInfoImpl = do
  fNr <- currentFontNumberImpl
  use @HexState (stateFontInfoLens fNr)

currentFontNumberImpl ::
  State HexState :> es =>
  Eff es DVI.FontNumber
currentFontNumberImpl =
  getGroupScopesProperty Sc.Font.localCurrentFontNr
