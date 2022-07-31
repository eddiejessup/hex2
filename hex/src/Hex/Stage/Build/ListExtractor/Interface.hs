{-# LANGUAGE TemplateHaskell #-}

module Hex.Stage.Build.ListExtractor.Interface where

import Hex.Stage.Build.ListElem (HList, VList)
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
  ExtractHBoxList :: ExtractList m HList
  ExtractVBoxList :: ExtractList m VList
  ExtractParagraphList :: IndentFlag -> ExtractList m (EndHListReason, HList)

makeEffect ''ExtractList
