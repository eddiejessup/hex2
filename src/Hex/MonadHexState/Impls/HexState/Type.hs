module Hex.MonadHexState.Impls.HexState.Type where

import Hex.Codes qualified as H.Codes
import Hex.MonadHexState.Impls.HexState.Parameters qualified as H.Inter.St.Param
import Hex.MonadHexState.Impls.HexState.Scope qualified as H.Inter.St.Scope
import Hex.Quantity qualified as H.Q
import Hex.Symbol.Token.Primitive qualified as H.Sym.Tok
import Hex.Symbol.Token.Resolved qualified as H.Sym.Tok
import Hex.Symbol.Types qualified as H.Sym
import Hex.TFM.Types qualified as H.TFM
import Hexlude

data HexState = HexState
  { fontInfos :: Map H.Sym.Tok.FontNumber FontInfo,
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

stateLocalResolvedToken :: H.Sym.ControlSymbol -> HexState -> Maybe H.Sym.Tok.ResolvedToken
stateLocalResolvedToken p = scopedLookup (view (H.Inter.St.Scope.scopeResolvedTokenLens p))

stateLocalCategory :: H.Codes.CharCode -> HexState -> H.Codes.CatCode
stateLocalCategory p = fromMaybe H.Codes.Invalid . scopedLookup (view (H.Inter.St.Scope.scopeCategoryLens p))

stateLocalIntParam :: H.Sym.Tok.IntParameter -> HexState -> H.Q.HexInt
stateLocalIntParam p = fromMaybe H.Q.zeroInt . scopedLookup (view (H.Inter.St.Scope.scopeIntParamLens p))

stateLocalLengthParam :: H.Sym.Tok.LengthParameter -> HexState -> H.Q.Length
stateLocalLengthParam p = fromMaybe H.Q.zeroLength . scopedLookup (view (H.Inter.St.Scope.scopeLengthParamLens p))

stateLocalGlueParam :: H.Sym.Tok.GlueParameter -> HexState -> H.Q.Glue
stateLocalGlueParam p = fromMaybe H.Q.zeroGlue . scopedLookup (view (H.Inter.St.Scope.scopeGlueParamLens p))

stateSpecialLengthParamLens :: H.Sym.Tok.SpecialLengthParameter -> Lens' HexState H.Q.Length
stateSpecialLengthParamLens p = #specialLengths % at' p % non (H.Q.Length 0)

stateCurrentFontNr :: HexState -> Maybe H.Sym.Tok.FontNumber
stateCurrentFontNr = scopedLookup (getField @"currentFontNr")

stateFontInfoLens :: H.Sym.Tok.FontNumber -> Lens' HexState (Maybe FontInfo)
stateFontInfoLens fNr = #fontInfos % at' fNr

selectFontNr :: H.Sym.Tok.FontNumber -> H.Sym.Tok.ScopeFlag -> HexState -> HexState
selectFontNr n scopeFlag st =
  case scopeFlag of
    H.Sym.Tok.Global ->
      st
        & #globalScope % #currentFontNr ?~ n
        & stateLocalScopesTraversal % #currentFontNr .~ Nothing
    H.Sym.Tok.Local ->
      st & stateLocalMostScopeLens % #currentFontNr ?~ n
