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

data HexListExtractor :: Effect where
  ExtractHBoxList :: HexListExtractor m HList
  ExtractParagraphList :: IndentFlag -> HexListExtractor m (EndHListReason, HList)

makeEffect ''HexListExtractor
