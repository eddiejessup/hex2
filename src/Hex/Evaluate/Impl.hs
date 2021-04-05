module Hex.Evaluate.Impl where

import Hex.Codes qualified as H.Codes
import Hex.Interpret.Build.Box.Elem qualified as H.Inter.B.Box
import Hex.Parse.AST.Command qualified as AST
import Hex.Parse.AST.Common qualified as AST
import Hex.Quantity qualified as H.Q
import Hexlude

data EvaluationError
  = CharCodeNotInRange
  deriving stock (Show, Generic)

evalASTInt :: Monad m => AST.HexInt -> m H.Q.HexInt
evalASTInt = undefined

evalASTLength :: Monad m => AST.Length -> m H.Q.Length
evalASTLength = undefined

evalASTGlue :: Monad m => AST.Glue -> m H.Q.Glue
evalASTGlue = undefined

evalASTRule ::
  ( Monad m
  ) =>
  AST.Rule ->
  m H.Q.Length ->
  m H.Q.Length ->
  m H.Q.Length ->
  m H.Inter.B.Box.Rule
evalASTRule (AST.Rule dims) defaultW defaultH defaultD =
  undefined

-- H.Inter.B.Box.Rule
--   <$> ( H.Inter.B.Box.Box ()
--           <$> maybe defaultW evalASTLength width
--           <*> maybe defaultH evalASTLength height
--           <*> maybe defaultD evalASTLength depth
--       )

evalASTVModeRule ::
  ( Monad m
  ) =>
  AST.Rule ->
  m H.Inter.B.Box.Rule
evalASTVModeRule rule =
  -- ruleToElem rule defaultWidth defaultHeight defaultDepth
  undefined

-- where
--   defaultWidth = use $ typed @Config % to (lookupLengthParameter HP.HSize)
--   defaultHeight = pure $ toScaledPointApprox (0.4 :: Rational) Point
--   defaultDepth = pure 0

evalASTHModeRule ::
  Monad m =>
  AST.Rule ->
  m H.Inter.B.Box.Rule
evalASTHModeRule rule =
  undefined

-- ruleToElem rule defaultWidth defaultHeight defaultDepth
-- where
--   defaultWidth = pure (toScaledPointApprox (0.4 :: Rational) Point)
--   defaultHeight = pure (toScaledPointApprox (10 :: Int) Point)
--   defaultDepth = pure 0

evalASTChar :: (MonadError e m, AsType EvaluationError e) => AST.CharCodeRef -> m H.Codes.CharCode
evalASTChar = \case
  AST.CharRef c -> pure c
  AST.CharTokenRef c -> noteRange c
  AST.CharCodeNrRef n -> evalASTInt n >>= noteRange
  where
    noteRange x =
      note (injectTyped CharCodeNotInRange) (H.Codes.fromHexInt x)
