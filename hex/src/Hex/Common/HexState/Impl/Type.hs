module Hex.Common.HexState.Impl.Type where

import Hex.Common.Codes qualified as H.Codes
import Hex.Common.HexState.Impl.Parameters qualified as H.Inter.St.Param
import Hex.Common.HexState.Impl.Scope qualified as H.Inter.St.Scope
import Hex.Common.HexState.Interface.Resolve (ControlSymbol, ResolvedToken)
import Hex.Common.HexState.Interface.Resolve.PrimitiveToken qualified as PT
import Hex.Common.Quantity qualified as H.Q
import Hex.Common.TFM.Types qualified as H.TFM
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
    specialInts :: Map PT.SpecialIntParameter H.Q.HexInt,
    specialLengths :: Map PT.SpecialLengthParameter H.Q.Length,
    -- afterAssignmentToken :: Maybe Lex.Token,
    -- Scopes and groups.
    globalScope :: H.Inter.St.Scope.Scope,
    groups :: [H.Inter.St.Scope.HexGroup]
  }
  deriving stock (Generic)

newHexState :: HexState
newHexState =
  HexState
    { fontInfos = mempty,
      specialInts = H.Inter.St.Param.newSpecialIntParameters,
      specialLengths = H.Inter.St.Param.newSpecialLengthParameters,
      -- , logStream = logHandle
      -- , outFileStreams = mempty
      -- , afterAssignmentToken = Nothing
      globalScope = H.Inter.St.Scope.newGlobalScope,
      groups = []
      -- , internalLoggerSet
    }

data FontInfo = FontInfo {fontMetrics :: H.TFM.Font, hyphenChar :: H.Q.HexInt, skewChar :: H.Q.HexInt}
  deriving stock (Show, Generic)

stateLocalScopesTraversal :: Traversal' HexState H.Inter.St.Scope.Scope
stateLocalScopesTraversal = #groups % H.Inter.St.Scope.groupListScopeTraversal

stateScopes :: HexState -> [H.Inter.St.Scope.Scope]
stateScopes HexState {globalScope, groups} = globalScope : toListOf H.Inter.St.Scope.groupListScopeTraversal groups

scopedLookup :: (H.Inter.St.Scope.Scope -> Maybe v) -> HexState -> Maybe v
scopedLookup f c = asum $ f <$> stateScopes c

stateLocalMostScopeLens :: Lens' HexState H.Inter.St.Scope.Scope
stateLocalMostScopeLens = lens getter setter
  where
    getter :: HexState -> H.Inter.St.Scope.Scope
    getter st@HexState {globalScope} = fromMaybe globalScope $ headOf stateLocalScopesTraversal st

    setter :: HexState -> H.Inter.St.Scope.Scope -> HexState
    setter c@HexState {groups} newScope =
      c & go [] groups
      where
        -- Traverse the groups to find the inner scope group whose scope we
        -- should replace, if any. Otherwise, we should update the global scope.
        go befGroups aftGroups = case aftGroups of
          -- If we've traversed all groups without finding a scope group, set
          -- the global scope to the new scope.
          [] ->
            #globalScope .~ newScope
          -- If we see a scope group, replace that with the new scope, keeping
          -- the groups before and after as before.
          H.Inter.St.Scope.ScopeGroup grpScope : restAftGroups ->
            #groups .~ (befGroups ++ (H.Inter.St.Scope.ScopeGroup grpScope {H.Inter.St.Scope.scgScope = newScope} : restAftGroups))
          -- If we see a non-scope group, add it to our befGroups stack and keep traversing the groups.
          nonScopeGroup : restAftGroups ->
            go (befGroups ++ [nonScopeGroup]) restAftGroups

stateLocalResolvedToken :: ControlSymbol -> HexState -> Maybe ResolvedToken
stateLocalResolvedToken p = scopedLookup (view (H.Inter.St.Scope.scopeResolvedTokenLens p))

stateLocalCategory :: H.Codes.CharCode -> HexState -> H.Codes.CatCode
stateLocalCategory p = fromMaybe H.Codes.Invalid . scopedLookup (view (H.Inter.St.Scope.scopeCategoryLens p))

stateLocalIntParam :: PT.IntParameter -> HexState -> H.Q.HexInt
stateLocalIntParam p = fromMaybe H.Q.zeroInt . scopedLookup (view (H.Inter.St.Scope.scopeIntParamLens p))

stateLocalLengthParam :: PT.LengthParameter -> HexState -> H.Q.Length
stateLocalLengthParam p = fromMaybe H.Q.zeroLength . scopedLookup (view (H.Inter.St.Scope.scopeLengthParamLens p))

stateLocalGlueParam :: PT.GlueParameter -> HexState -> H.Q.Glue
stateLocalGlueParam p = fromMaybe H.Q.zeroGlue . scopedLookup (view (H.Inter.St.Scope.scopeGlueParamLens p))

stateSpecialLengthParamLens :: PT.SpecialLengthParameter -> Lens' HexState H.Q.Length
stateSpecialLengthParamLens p = #specialLengths % at' p % non (H.Q.Length 0)

stateCurrentFontNr :: HexState -> Maybe PT.FontNumber
stateCurrentFontNr = scopedLookup (getField @"currentFontNr")

stateFontInfoLens :: PT.FontNumber -> Lens' HexState (Maybe FontInfo)
stateFontInfoLens fNr = #fontInfos % at' fNr

selectFontNr :: PT.FontNumber -> PT.ScopeFlag -> HexState -> HexState
selectFontNr n scopeFlag st =
  case scopeFlag of
    PT.Global ->
      st
        & #globalScope % #currentFontNr ?~ n
        & stateLocalScopesTraversal % #currentFontNr .~ Nothing
    PT.Local ->
      st & stateLocalMostScopeLens % #currentFontNr ?~ n
