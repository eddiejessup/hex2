{-# LANGUAGE TemplateHaskell #-}

module Hex.Stage.Evaluate.Interface where

import Hex.Stage.Evaluate.Interface.AST.Command qualified as Eval
import Hex.Stage.Parse.Interface (CommandSource (..), getCommand)
import Hex.Stage.Parse.Interface.AST.Command qualified as Uneval
import Hexlude

data HexEvaluate :: Effect where
  EvalCommand :: Uneval.Command -> HexEvaluate m Eval.Command

makeEffect ''HexEvaluate

getEvalCommand :: [CommandSource, HexEvaluate] :>> es => Eff es (Maybe Eval.Command)
getEvalCommand =
  getCommand >>= \case
    Nothing -> pure Nothing
    Just c -> Just <$> evalCommand c
