{-# LANGUAGE TemplateHaskell #-}

module Hex.Stage.Build.ListExtractor.Interface where

import Hex.Stage.Build.ListElem (HList)
import Hexlude

data IndentFlag
  = Indent
  | DoNotIndent
  deriving stock (Show, Eq, Generic)

data EndHListReason
  = EndHListSawEndParaCommand
  | EndHListSawLeaveBox
  deriving stock (Show, Generic)

data ModeContext = InnerModeContext | OuterModeContext
  deriving stock (Show, Generic)

data ExtractHList :: Effect where
  ExtractHBoxList :: ExtractHList m HList
  ExtractParagraphList :: IndentFlag -> ExtractHList m (EndHListReason, HList)

makeEffect ''ExtractHList
