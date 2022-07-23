{-# LANGUAGE TemplateHaskell #-}

module Hex.Stage.Parse.Interface where

import Hex.Stage.Parse.Interface.AST.Command (Command)
import Hexlude

data CommandSource :: Effect where
  GetCommand :: CommandSource m (Maybe Command)

makeEffect ''CommandSource
