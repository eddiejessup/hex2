{-# LANGUAGE TemplateHaskell #-}

module Hex.Stage.Build.ListExtractor.Interface where

import Hex.Stage.Build.ListElem (HListElem, VListElem)
import Hexlude

data IndentFlag
  = Indent
  | DoNotIndent
  deriving stock (Show, Eq, Generic)

data EndHListReason
  = EndHListSawEndParaCommand
  | EndHListSawLeaveBox
  deriving stock (Show, Generic)

data ExtractList :: Effect where
  ExtractHBoxList :: ExtractList m (Seq HListElem)
  ExtractVBoxList :: ExtractList m (Seq VListElem)
  ExtractParagraphList :: IndentFlag -> ExtractList m (EndHListReason, Seq HListElem)

makeEffect ''ExtractList
