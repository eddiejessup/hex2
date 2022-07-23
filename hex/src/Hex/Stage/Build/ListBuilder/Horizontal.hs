{-# LANGUAGE UndecidableInstances #-}

module Hex.Stage.Build.ListBuilder.Horizontal where

import Hex.Stage.Build.ListBuilder.Interface
import Hex.Stage.Build.ListElem qualified as ListElem
import Hexlude

addHListElementImpl :: (State ListElem.HList :> es) => ListElem.HListElem -> Eff es ()
addHListElementImpl e = do
  hList <- get
  newHList <- extendHList e hList
  put newHList

-- | This doesn't need to be monadic, but it probably will be at some point, and
-- this keeps things symmetric with the 'vlist' case.
extendHList ::
  Monad m =>
  ListElem.HListElem ->
  ListElem.HList ->
  m ListElem.HList
extendHList e (ListElem.HList accSeq) =
  pure $ ListElem.HList $ accSeq :|> e

runHListBuilder :: State ListElem.HList :> es => Eff (HListBuilder : es) a -> Eff es a
runHListBuilder = interpret $ \_ -> \case
  AddHListElement e -> addHListElementImpl e

runHexListBuilderHMode :: State ListElem.HList :> es => Eff (HexListBuilder : es) a -> Eff es a
runHexListBuilderHMode = interpret $ \_ -> \case
  AddVListElement e -> addHListElementImpl (ListElem.HVListElem e)
