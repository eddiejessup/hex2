{-# LANGUAGE TemplateHaskell #-}

module Hex.Stage.Build.ListBuilder.Interface where

import Hex.Stage.Build.ListElem qualified as B
import Hexlude

data HexListBuilder :: Effect where
  AddVListElement :: B.VListElem -> HexListBuilder m ()

makeEffect ''HexListBuilder

data HListBuilder :: Effect where
  AddHListElement :: B.HListElem -> HListBuilder m ()
  AddUnboxedHListElements :: Seq B.HListElem -> HListBuilder m ()
  GetLastHListElement :: HListBuilder m (Maybe B.HListElem)

makeEffect ''HListBuilder
