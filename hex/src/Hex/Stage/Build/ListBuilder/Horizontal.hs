{-# LANGUAGE UndecidableInstances #-}

module Hex.Stage.Build.ListBuilder.Horizontal where

import Hex.Common.HexState.Interface qualified as HSt
import Hex.Common.HexState.Interface.Parameter qualified as HSt.Param
import Hex.Common.Quantity qualified as Q
import Hex.Stage.Build.BoxElem qualified as BoxElem
import Hex.Stage.Build.ListBuilder.Interface
import Hex.Stage.Build.ListElem qualified as ListElem
import Hexlude

addHListElementImpl :: (State ListElem.HList :> es, HSt.EHexState :> es) => ListElem.HListElem -> Eff es ()
addHListElementImpl e = do
  hList <- get
  newHList <- extendHList e hList
  put newHList
  -- [The space factor] is set to 1000 just after a non-character box
  -- or a math formula has been put onto the current horizontal list.
  case e of
    ListElem.HListHBaseElem (BoxElem.CharBoxHBaseElem _) ->
      pure ()
    ListElem.HVListElem (ListElem.ListGlue _) ->
      pure ()
    ListElem.HVListElem (ListElem.ListPenalty _) ->
      pure ()
    ListElem.HVListElem (ListElem.VListBaseElem (BoxElem.AxOrRuleBoxBaseElem _)) ->
      HSt.setSpecialIntParameter HSt.Param.SpaceFactor Q.thousandInt
    ListElem.HVListElem (ListElem.VListBaseElem _) ->
      pure ()
    ListElem.DiscretionaryItemElem _ ->
      pure ()

-- | This doesn't need to be monadic, but it probably will be at some point, and
-- this keeps things symmetric with the 'vlist' case.
extendHList ::
  Monad m =>
  ListElem.HListElem ->
  ListElem.HList ->
  m ListElem.HList
extendHList e (ListElem.HList accSeq) =
  pure $ ListElem.HList $ accSeq :|> e

runHListBuilder :: (State ListElem.HList :> es, HSt.EHexState :> es) => Eff (HListBuilder : es) a -> Eff es a
runHListBuilder = interpret $ \_ -> \case
  AddHListElement e -> addHListElementImpl e
  GetLastHListElement ->
    lastOf ListElem.hListElemTraversal <$> get

runHexListBuilderHMode :: (State ListElem.HList :> es, HSt.EHexState :> es) => Eff (HexListBuilder : es) a -> Eff es a
runHexListBuilderHMode = interpret $ \_ -> \case
  AddVListElement e -> do
    addHListElementImpl (ListElem.HVListElem e)
