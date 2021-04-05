module Hex.HexState.Type where

import Hex.Codes qualified as H.Codes
import Hex.HexState.Parameters qualified as H.Inter.St.Param
import Hex.HexState.Scope
import Hex.MonadHexState.Interface qualified as H.MSt
import Hex.Quantity qualified as H.Q
import Hex.Symbol.Tokens qualified as H.Sym.Tok
import Hex.Symbol.Types qualified as H.Sym
import Hex.TFM.Types qualified as H.TFM
import Hexlude

data HexState = HexState
  { fontInfos :: Map H.MSt.FontNumber FontInfo,
    --searchDirectories :: [Path Abs Dir],
    -- File streams.
    -- logStream :: Handle,
    -- outFileStreams :: Map FourBitInt Handle,
    -- internalLoggerSet :: Log.LoggerSet,
    -- / File streams.
    -- Global parameters.
    specialInts :: Map H.Sym.Tok.SpecialIntParameter H.Q.HexInt,
    specialLengths :: Map H.Sym.Tok.SpecialLengthParameter H.Q.Length,
    -- afterAssignmentToken :: Maybe Lex.Token,
    -- Scopes and groups.
    globalScope :: Scope,
    groups :: [HexGroup]
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
      globalScope = newGlobalScope,
      groups = []
      -- , internalLoggerSet
    }

data FontInfo = FontInfo {fontMetrics :: H.TFM.Font, hyphenChar :: H.Q.HexInt, skewChar :: H.Q.HexInt}
  deriving stock (Show, Generic)

stateLocalScopesTraversal :: Traversal' HexState Scope
stateLocalScopesTraversal = #groups % groupListScopeTraversal

stateScopes :: HexState -> [Scope]
stateScopes HexState {globalScope, groups} = globalScope : toListOf groupListScopeTraversal groups

scopedLookup :: (Scope -> Maybe v) -> HexState -> Maybe v
scopedLookup f c = asum $ f <$> stateScopes c

stateLocalMostScopeLens :: Lens' HexState Scope
stateLocalMostScopeLens = lens getter setter
  where
    getter :: HexState -> Scope
    getter st@HexState {globalScope} = fromMaybe globalScope $ headOf stateLocalScopesTraversal st

    setter :: HexState -> Scope -> HexState
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
          ScopeGroup grpScope : restAftGroups ->
            #groups .~ (befGroups ++ (ScopeGroup grpScope {scgScope = newScope} : restAftGroups))
          -- If we see a non-scope group, add it to our befGroups stack and keep traversing the groups.
          nonScopeGroup : restAftGroups ->
            go (befGroups ++ [nonScopeGroup]) restAftGroups

stateLocalResolvedToken :: H.Sym.ControlSymbol -> HexState -> Maybe H.Sym.Tok.ResolvedToken
stateLocalResolvedToken p = scopedLookup (view (scopeResolvedTokenLens p))

stateLocalCategory :: H.Codes.CharCode -> HexState -> H.Codes.CatCode
stateLocalCategory p = fromMaybe H.Codes.Invalid . scopedLookup (view (scopeCategoryLens p))

stateLocalIntParam :: H.Sym.Tok.IntParameter -> HexState -> H.Q.HexInt
stateLocalIntParam p = fromMaybe H.Q.zeroInt . scopedLookup (view (scopeIntParamLens p))

stateLocalLengthParam :: H.Sym.Tok.LengthParameter -> HexState -> H.Q.Length
stateLocalLengthParam p = fromMaybe H.Q.zeroLength . scopedLookup (view (scopeLengthParamLens p))

stateLocalGlueParam :: H.Sym.Tok.GlueParameter -> HexState -> H.Q.Glue
stateLocalGlueParam p = fromMaybe H.Q.zeroGlue . scopedLookup (view (scopeGlueParamLens p))

stateSpecialLengthParamLens :: H.Sym.Tok.SpecialLengthParameter -> Lens' HexState H.Q.Length
stateSpecialLengthParamLens p = #specialLengths % at' p % non (H.Q.Length 0)

stateCurrentFontNr :: HexState -> Maybe H.MSt.FontNumber
stateCurrentFontNr = scopedLookup (getField @"currentFontNr")

stateFontInfoLens :: H.MSt.FontNumber -> Lens' HexState (Maybe FontInfo)
stateFontInfoLens fNr = #fontInfos % at' fNr

selectFontNr :: H.MSt.FontNumber -> H.Sym.Tok.ScopeFlag -> HexState -> HexState
selectFontNr n scopeFlag st =
  case scopeFlag of
    H.Sym.Tok.Global ->
      st
        & #globalScope % #currentFontNr ?~ n
        & stateLocalScopesTraversal % #currentFontNr .~ Nothing
    H.Sym.Tok.Local ->
      st & stateLocalMostScopeLens % #currentFontNr ?~ n
