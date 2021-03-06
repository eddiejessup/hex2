module Hex.Interpret.Evaluate.Impl where

import Hex.Interpret.Build.Box.Elem qualified as H.Inter.B.Box
import Hex.Interpret.Evaluate.Evaluated qualified as E
import Hex.Parse.AST qualified as AST
import Protolude
import Hex.Codes qualified as H.Codes

evalASTGlue :: Monad m => AST.Glue -> m (E.Glue E.Length)
evalASTGlue = undefined

evalASTLength :: Monad m => AST.Length -> m E.Length
evalASTLength = undefined

evalASTRule ::
  ( Monad m
  ) =>
  AST.Rule ->
  m E.Length ->
  m E.Length ->
  m E.Length ->
  m H.Inter.B.Box.Rule
evalASTRule AST.Rule {AST.width, AST.height, AST.depth} defaultW defaultH defaultD =
  H.Inter.B.Box.Rule
    <$> maybe defaultW evalASTLength width
    <*> maybe defaultH evalASTLength height
    <*> maybe defaultD evalASTLength depth

evalASTVModeRule
    :: ( Monad m
       )
    => AST.Rule
    -> m H.Inter.B.Box.Rule
evalASTVModeRule rule =
    -- ruleToElem rule defaultWidth defaultHeight defaultDepth
    undefined
  -- where
  --   defaultWidth = use $ typed @Config % to (lookupLengthParameter HP.HSize)
  --   defaultHeight = pure $ toScaledPointApprox (0.4 :: Rational) Point
  --   defaultDepth = pure 0

evalASTHModeRule
    :: Monad m
    => AST.Rule
    -> m H.Inter.B.Box.Rule
evalASTHModeRule rule =
  undefined
    -- ruleToElem rule defaultWidth defaultHeight defaultDepth
  -- where
  --   defaultWidth = pure (toScaledPointApprox (0.4 :: Rational) Point)
  --   defaultHeight = pure (toScaledPointApprox (10 :: Int) Point)
  --   defaultDepth = pure 0

evalASTChar :: Monad m => AST.CharCodeRef -> m H.Codes.CharCode
evalASTChar = undefined
