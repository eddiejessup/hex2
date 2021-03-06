module Hex.HexState.Type where

import Data.Generics.Product qualified as G.P
import Hex.Interpret.Evaluate.Evaluated qualified as H.Inter.Eval
import Hex.HexState.Parameters qualified as H.Inter.St.Param
import Hex.HexState.Scope
import Hex.Symbol.Tokens qualified as H.Sym.Tok
import Optics.Core ((%), (.~))
import Optics.Core qualified as O
import Protolude hiding ((%))
import Hex.Codes qualified as H.Codes
import Hex.Lex.Types qualified as H.Lex

data HexState = HexState
  { --fontInfos :: Map HexInt FontInfo,
    --searchDirectories :: [Path Abs Dir],
    -- File streams.
    -- logStream :: Handle,
    -- outFileStreams :: Map FourBitInt Handle,
    -- internalLoggerSet :: Log.LoggerSet,
    -- / File streams.
    -- Global parameters.
    specialInts :: Map H.Sym.Tok.SpecialIntParameter H.Inter.Eval.HexInt,
    specialLengths :: Map H.Sym.Tok.SpecialLengthParameter H.Inter.Eval.Length,
    -- afterAssignmentToken :: Maybe Lex.Token,
    -- Scopes and groups.
    globalScope :: Scope,
    groups :: [Group]
  }
  deriving stock (Generic)

newHexState :: HexState
newHexState =
  HexState
    { specialInts = H.Inter.St.Param.newSpecialIntParameters,
      specialLengths = H.Inter.St.Param.newSpecialLengthParameters,
      -- , logStream = logHandle
      -- , outFileStreams = mempty
      -- , afterAssignmentToken = Nothing
      globalScope = newGlobalScope,
      groups = []
      -- , internalLoggerSet
    }

stateScopes :: HexState -> [Scope]
stateScopes HexState{ globalScope, groups } = globalScope : groupScopes groups

scopedLookup :: (Scope -> Maybe v) -> HexState -> Maybe v
scopedLookup f c = asum $ f <$> stateScopes c

stateLocalScopeLens :: O.Lens' HexState Scope
stateLocalScopeLens = O.lens getter setter
  where
    getter :: HexState -> Scope
    getter HexState {globalScope, groups} = fromMaybe globalScope $ asum $ groupScope <$> groups

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
            G.P.field @"globalScope" .~ newScope
          -- If we see a scope group, replace that with the new scope, keeping
          -- the groups before and after as before.
          ScopeGroup _ scopeGroupType : restAftGroups ->
            G.P.field @"groups" .~ (befGroups ++ (ScopeGroup newScope scopeGroupType : restAftGroups))
          -- If we see a non-scope group, add it to our befGroups stack and keep traversing the groups.
          nonScopeGroup : restAftGroups ->
            go (befGroups ++ [nonScopeGroup]) restAftGroups

stateLocalResolvedToken :: H.Lex.LexSymbol -> HexState -> Maybe H.Sym.Tok.ResolvedToken
stateLocalResolvedToken p = scopedLookup (O.view (scopeResolvedTokenLens p))

stateLocalCategory :: H.Codes.CharCode -> HexState -> H.Codes.CatCode
stateLocalCategory p = fromMaybe H.Codes.Invalid . scopedLookup (O.view (scopeCategoryLens p))

stateLocalIntParam :: H.Sym.Tok.IntParameter -> HexState -> H.Inter.Eval.HexInt
stateLocalIntParam p = fromMaybe 0 . scopedLookup (O.view (scopeIntParamLens p))

stateLocalLengthParam :: H.Sym.Tok.LengthParameter -> HexState -> H.Inter.Eval.Length
stateLocalLengthParam p = fromMaybe 0 . scopedLookup (O.view (scopeLengthParamLens p))

stateLocalGlueParam :: H.Sym.Tok.GlueParameter -> HexState -> H.Inter.Eval.Glue H.Inter.Eval.Length
stateLocalGlueParam p = fromMaybe mempty . scopedLookup (O.view (scopeGlueParamLens p))

stateSpecialLengthParamLens :: H.Sym.Tok.SpecialLengthParameter -> O.Lens' HexState H.Inter.Eval.Length
stateSpecialLengthParamLens p = G.P.field @"specialLengths" % O.at' p % O.non (H.Inter.Eval.Length 0)
