module Hex.Common.HexState.Interface where

import Hex.Common.Codes qualified as Code
import Hex.Common.Codes qualified as Codes
import Hex.Common.HexState.Impl.Scoped.Code (MutableHexCode)
import Hex.Common.HexState.Impl.Scoped.Parameter (ScopedHexParameter (..))
import Hex.Common.HexState.Impl.Scoped.Register (ScopedHexRegisterValue (..))
import Hex.Common.HexState.Impl.Scoped.Scope qualified as Scope
import Hex.Common.HexState.Interface.Resolve (ControlSymbol, ResolvedToken)
import Hex.Common.HexState.Interface.Resolve.PrimitiveToken qualified as PT
import Hex.Common.Quantity qualified as Q
import Hex.Stage.Interpret.Build.Box.Elem qualified as H.Inter.B.Box
import Hex.Stage.Interpret.Build.List.Elem qualified as H.Inter.B.List
import Hex.Stage.Lex.Interface.Extract qualified as Lex
import Hexlude

class Monad m => MonadHexState m where
  getParameterValue :: ScopedHexParameter p => p -> m (ScopedHexParameterValue p)

  setParameterValue :: ScopedHexParameter p => p -> (ScopedHexParameterValue p) -> PT.ScopeFlag -> m ()

  getRegisterValue :: ScopedHexRegisterValue r => Scope.RegisterLocation -> m r

  setRegisterValue :: ScopedHexRegisterValue r => Scope.RegisterLocation -> r -> PT.ScopeFlag -> m ()

  getSpecialIntParameter :: PT.SpecialIntParameter -> m Q.HexInt

  getSpecialLengthParameter :: PT.SpecialLengthParameter -> m Q.Length

  setSpecialIntParameter :: PT.SpecialIntParameter -> Q.HexInt -> m ()

  setSpecialLengthParameter :: PT.SpecialLengthParameter -> Q.Length -> m ()

  getHexCode :: MutableHexCode c => Codes.CharCode -> m c

  setHexCode :: MutableHexCode c => Code.CharCode -> c -> PT.ScopeFlag -> m ()

  resolveSymbol :: ControlSymbol -> m (Maybe ResolvedToken)

  setSymbol :: ControlSymbol -> ResolvedToken -> PT.ScopeFlag -> m ()

  loadFont :: H.Inter.B.Box.HexFilePath -> H.Inter.B.Box.FontSpecification -> m H.Inter.B.Box.FontDefinition

  selectFont :: PT.FontNumber -> PT.ScopeFlag -> m ()

  currentFontCharacter :: Codes.CharCode -> m (Maybe (Q.Length, Q.Length, Q.Length, Q.Length))

  currentFontSpaceGlue :: m (Maybe Q.Glue)

  popAfterAssignmentToken :: m (Maybe Lex.LexToken)

  setAfterAssignmentToken :: Lex.LexToken -> m ()

  -- Support stuff for parsing.
  setLastFetchedLexTok :: Lex.LexToken -> m ()

  getLastFetchedLexTok :: m (Maybe Lex.LexToken)

instance MonadHexState m => MonadHexState (StateT a m) where
  getParameterValue x = lift $ getParameterValue x
  setParameterValue x y z = lift $ setParameterValue x y z
  getRegisterValue x = lift $ getRegisterValue x
  setRegisterValue x y z = lift $ setRegisterValue x y z
  getSpecialIntParameter x = lift $ getSpecialIntParameter x
  getSpecialLengthParameter x = lift $ getSpecialLengthParameter x
  setSpecialIntParameter x y = lift $ setSpecialIntParameter x y
  setSpecialLengthParameter x y = lift $ setSpecialLengthParameter x y
  getHexCode x = lift $ getHexCode x
  setHexCode x y z = lift $ setHexCode x y z
  resolveSymbol x = lift $ resolveSymbol x
  loadFont x y = lift $ loadFont x y
  selectFont x y = lift $ selectFont x y
  currentFontCharacter x = lift $ currentFontCharacter x
  currentFontSpaceGlue = lift currentFontSpaceGlue
  popAfterAssignmentToken = lift popAfterAssignmentToken
  setAfterAssignmentToken x = lift $ setAfterAssignmentToken x
  setSymbol x y z = lift $ setSymbol x y z
  setLastFetchedLexTok x = lift $ setLastFetchedLexTok x
  getLastFetchedLexTok = lift getLastFetchedLexTok

getParIndentBox :: MonadHexState m => m H.Inter.B.List.HListElem
getParIndentBox = do
  boxWidth <- getParameterValue @_ @PT.LengthParameter PT.ParIndent
  pure $
    H.Inter.B.List.HVListElem $
      H.Inter.B.List.VListBaseElem $
        H.Inter.B.Box.ElemBox $
          H.Inter.B.Box.Box
            { H.Inter.B.Box.contents = H.Inter.B.Box.HBoxContents (H.Inter.B.Box.HBoxElemSeq Empty),
              H.Inter.B.Box.boxWidth,
              H.Inter.B.Box.boxHeight = mempty,
              H.Inter.B.Box.boxDepth = mempty
            }

modifyParameterValue ::
  (MonadHexState m, ScopedHexParameter p) =>
  p ->
  ( ScopedHexParameterValue p ->
    ScopedHexParameterValue p
  ) ->
  PT.ScopeFlag ->
  m ()
modifyParameterValue p f scopeFlag = do
  currentVal <- getParameterValue p
  setParameterValue p (f currentVal) scopeFlag

modifyRegisterValue ::
  (MonadHexState m, ScopedHexRegisterValue r) =>
  Scope.RegisterLocation ->
  (r -> r) ->
  PT.ScopeFlag ->
  m ()
modifyRegisterValue loc f scopeFlag = do
  currentVal <- getRegisterValue loc
  setRegisterValue loc (f currentVal) scopeFlag

advanceParameterValue :: (MonadHexState m, ScopedHexParameter p) => p -> (ScopedHexParameterValue p) -> PT.ScopeFlag -> m ()
advanceParameterValue p plusVal =
  modifyParameterValue p (\v -> v <> plusVal)

scaleParameterValue ::
  (MonadHexState m, ScopedHexParameter p) =>
  p ->
  Q.VDirection ->
  Q.HexInt ->
  PT.ScopeFlag ->
  m ()
scaleParameterValue p scaleDirection arg scopeFlag =
  modifyParameterValue p (Q.scaleInDirection scaleDirection arg) scopeFlag

advanceRegisterValue ::
  (MonadHexState m, ScopedHexRegisterValue r) =>
  Scope.RegisterLocation ->
  r ->
  PT.ScopeFlag ->
  m ()
advanceRegisterValue loc plusVal =
  modifyRegisterValue loc (\v -> v <> plusVal)

scaleRegisterValue ::
  (MonadHexState m) =>
  PT.NumericQuantityType ->
  Scope.RegisterLocation ->
  Q.VDirection ->
  Q.HexInt ->
  PT.ScopeFlag ->
  m ()
scaleRegisterValue qType loc scaleDirection arg scopeFlag =
  case qType of
    PT.IntNumericQuantity ->
      modifyRegisterValue loc (Q.scaleInDirection @Q.HexInt scaleDirection arg) scopeFlag
    PT.LengthNumericQuantity ->
      modifyRegisterValue loc (Q.scaleInDirection @Q.Length scaleDirection arg) scopeFlag
    PT.GlueNumericQuantity ->
      modifyRegisterValue loc (Q.scaleInDirection @Q.Glue scaleDirection arg) scopeFlag
    PT.MathGlueNumericQuantity ->
      modifyRegisterValue loc (Q.scaleInDirection @Q.MathGlue scaleDirection arg) scopeFlag
