{-# LANGUAGE UndecidableInstances #-}

module Hex.Stage.Build.ListBuilder.Vertical where

import Control.Monad.Trans (MonadTrans (..))
import Data.Sequence qualified as Seq
import Formatting qualified as F
import Hex.Capability.Log.Interface (MonadHexLog (..))
import Hex.Capability.Log.Interface qualified as Log
import Hex.Common.Box qualified as Box
import Hex.Common.HexInput.Interface qualified as HIn
import Hex.Common.HexState.Interface (MonadHexState)
import Hex.Common.HexState.Interface qualified as HSt
import Hex.Common.HexState.Interface.Parameter qualified as HSt.Param
import Hex.Common.Quantity qualified as Q
import Hex.Stage.Build.AnyDirection.Breaking.Types qualified as Break
import Hex.Stage.Build.BoxElem qualified as Box
import Hex.Stage.Build.ListBuilder.Interface
import Hex.Stage.Build.ListElem qualified as List
import Hex.Stage.Evaluate.Interface (MonadEvaluate (..))
import Hex.Stage.Parse.Interface (MonadCommandSource (..))
import Hexlude

newtype VListBuilderT m a = VListBuilderT {unVListBuilderT :: StateT List.VList m a}
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

runVListBuilderT :: List.VList -> VListBuilderT m a -> m (a, List.VList)
runVListBuilderT initVList app = runStateT (unVListBuilderT app) initVList

execVListBuilderT :: Monad m => List.VList -> VListBuilderT m a -> m List.VList
execVListBuilderT initVList app = execStateT (unVListBuilderT app) initVList

instance MonadTrans VListBuilderT where
  lift = VListBuilderT . lift

instance (HSt.MonadHexState m, Log.MonadHexLog m) => MonadHexListBuilder (VListBuilderT m) where
  addVListElement = addVListElementImpl

addVListElementImpl :: (HSt.MonadHexState m, Log.MonadHexLog m) => List.VListElem -> VListBuilderT m ()
addVListElementImpl e = do
  vList <- VListBuilderT $ get
  newVList <- lift $ extendVList e vList
  VListBuilderT $ put newVList

extendVList ::
  (HSt.MonadHexState m, Log.MonadHexLog m) =>
  List.VListElem ->
  List.VList ->
  m List.VList
extendVList e vList@(List.VList accSeq) = case e of
  -- TODO: topskip
  List.VListBaseElem (Box.ElemBox b) -> do
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
    HSt.setSpecialLengthParameter HSt.Param.PrevDepth (b.unBaseBox.boxDepth)
    if prevDepth <= (invert Q.oneKPt)
      then do
        Log.infoLog $ "extendVList: Adding box element without interline glue because prevDepth is " <> F.sformat Q.fmtLengthWithUnit prevDepth
        pure $ List.VList $ accSeq :|> e
      else do
        let proposedBaselineLength = blineGlue.gDimen ~~ prevDepth ~~ b.unBaseBox.boxHeight
        Log.infoLog $ "extendVList: Proposed baseline length: " <> F.sformat Q.fmtLengthWithUnit proposedBaselineLength <> ", skip limit: " <> F.sformat Q.fmtLengthWithUnit skipLimit
        -- Intuition: set the distance between baselines to \baselineskip, but no
        -- closer than \lineskiplimit [theBaselineLengthMin], in which case
        -- \lineskip [theMinBaselineGlue] is used.
        glue <-
          if proposedBaselineLength >= skipLimit
            then do
              let res = blineGlue & #gDimen !~ proposedBaselineLength
              Log.infoLog $ "extendVList: proposedBaselineLength >= skipLimit, so using BaselineSkip with length set to proposedBaselineLength, ie: " <> F.sformat Q.fmtGlue res
              pure res
            else do
              Log.infoLog $ "extendVList: proposedBaselineLength < skipLimit, so using LineSkip ie: " <> F.sformat Q.fmtGlue skip
              pure skip
        Log.infoLog $ "extendVList: Adding interline glue: " <> F.sformat Q.fmtGlue glue
        let glueElem = List.ListGlue glue
        pure $ List.VList $ accSeq |> glueElem |> e
  _ -> do
    if not (List.vListContainsBoxes vList) && (Break.vListElemIsDiscardable e)
      then do
        Log.infoLog $ "extendVList: Skipping adding element: " <> F.sformat List.fmtVListElem e <> ", to vertical list without boxes, length " <> show (Seq.length accSeq)
        pure (List.VList (accSeq))
      else do
        Log.infoLog $ "extendVList: Adding non-box element: " <> F.sformat List.fmtVListElem e
        pure (List.VList (accSeq :|> e))
