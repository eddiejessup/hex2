{-# LANGUAGE UndecidableInstances #-}

module Hex.Stage.Build.ListBuilder.Vertical where

import Control.Monad.Trans (MonadTrans (..))
import Hex.Capability.Log.Interface (MonadHexLog (..))
import Hex.Common.HexState.Interface (MonadHexState)
import Hex.Common.HexState.Interface qualified as HSt
import Hex.Common.HexState.Interface.Parameter qualified as HSt.Param
import Hex.Common.Quantity qualified as Q
import Hex.Stage.Build.BoxElem qualified as H.Inter.B.Box
import Hex.Stage.Build.ListBuilder.Interface
import Hex.Stage.Build.ListElem qualified as H.Inter.B.List
import Hex.Stage.Evaluate.Interface (MonadEvaluate (..))
import Hex.Stage.Parse.Interface (MonadCommandSource (..))
import Hexlude
import qualified Hex.Common.HexInput.Interface as HIn

newtype VListBuilderT m a = VListBuilderT {unVListBuilderT :: StateT H.Inter.B.List.VList m a}
  deriving newtype
    ( Functor,
      Applicative,
      Monad,
      MonadIO,
      MonadReader r,
      MonadError e,
      MonadHexState,
      MonadCommandSource,
      MonadEvaluate,
      MonadHexLog
    )

instance HIn.MonadHexInput m => HIn.MonadHexInput (VListBuilderT m) where
  endCurrentLine = lift HIn.endCurrentLine

  inputIsFinished = lift HIn.inputIsFinished

  getInput = lift HIn.getInput

  putInput = lift . HIn.putInput

  insertLexToken = lift . HIn.insertLexToken

  insertLexTokens = lift . HIn.insertLexTokens

  getNextLexToken = lift HIn.getNextLexToken

  openInputFile x = lift $ HIn.openInputFile x

runVListBuilderT :: H.Inter.B.List.VList -> VListBuilderT m a -> m (a, H.Inter.B.List.VList)
runVListBuilderT initVList app = runStateT (unVListBuilderT app) initVList

execVListBuilderT :: Monad m => H.Inter.B.List.VList -> VListBuilderT m a -> m H.Inter.B.List.VList
execVListBuilderT initVList app = execStateT (unVListBuilderT app) initVList

instance MonadTrans VListBuilderT where
  lift = VListBuilderT . lift

instance (HSt.MonadHexState m) => MonadHexListBuilder (VListBuilderT m) where
  addVListElement = addVListElementImpl

addVListElementImpl :: HSt.MonadHexState m => H.Inter.B.List.VListElem -> VListBuilderT m ()
addVListElementImpl e = do
  vList <- VListBuilderT $ get
  newVList <- lift $ extendVList e vList
  VListBuilderT $ put newVList

extendVList ::
  (HSt.MonadHexState m) =>
  H.Inter.B.List.VListElem ->
  H.Inter.B.List.VList ->
  m H.Inter.B.List.VList
extendVList e (H.Inter.B.List.VList accSeq) = case e of
  H.Inter.B.List.VListBaseElem (H.Inter.B.Box.ElemBox b) -> do
    -- Assume we are adding a non-rule box of height h to the vertical list.
    -- Let \prevdepth = p, \lineskiplimit = l, \baselineskip = (b plus y minus z).
    -- Add interline glue, above the new box, of:
    -- If p ≤ −1000 pt:
    --    No glue.
    -- Otherwise, if b−p−h ≥ l:
    --    (b−p−h) plus y minus z
    -- Otherwise:
    --    \lineskip
    -- Then set \prevdepth to the depth of the new box.
    prevDepth <- HSt.getSpecialLengthParameter HSt.Param.PrevDepth
    blineGlue <- HSt.getParameterValue (HSt.Param.GlueQuantParam HSt.Param.BaselineSkip)
    skipLimit <- HSt.getParameterValue (HSt.Param.LengthQuantParam HSt.Param.LineSkipLimit)
    skip <- HSt.getParameterValue (HSt.Param.GlueQuantParam HSt.Param.LineSkip)
    HSt.setSpecialLengthParameter HSt.Param.PrevDepth (H.Inter.B.Box.boxDepth b)
    pure $
      H.Inter.B.List.VList $
        if (prevDepth ^. typed @Int) <= -(Q.oneKPt ^. typed @Int)
          then accSeq :|> e
          else
            let proposedBaselineLength = (blineGlue ^. #gDimen) ~~ prevDepth ~~ H.Inter.B.Box.boxHeight b
                -- Intuition: set the distance between baselines to \baselineskip, but no
                -- closer than \lineskiplimit [theBaselineLengthMin], in which case
                -- \lineskip [theMinBaselineGlue] is used.
                glue =
                  H.Inter.B.List.ListGlue $
                    if proposedBaselineLength >= skipLimit
                      then blineGlue & #gDimen !~ proposedBaselineLength
                      else skip
             in (accSeq :|> glue) :|> e
  _ ->
    pure (H.Inter.B.List.VList (accSeq :|> e))
