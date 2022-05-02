module Hex.Stage.Evaluate.Impl.Eval where

import Hex.Common.Codes qualified as H.Codes
import Hex.Stage.Interpret.Build.Box.Elem qualified as H.Inter.B.Box
import Hex.Stage.Parse.Interface.AST.Command qualified as AST
import Hex.Stage.Parse.Interface.AST.Common qualified as AST
import Hex.Common.Quantity qualified as H.Q
import Hexlude

data EvaluationError
  = CharCodeNotInRange
  deriving stock (Show, Generic)

evalInt :: Monad m => AST.HexInt -> m H.Q.HexInt
evalInt = undefined

evalLength :: Monad m => AST.Length -> m H.Q.Length
evalLength = undefined

evalGlue :: Monad m => AST.Glue -> m H.Q.Glue
evalGlue = undefined

evalRule ::
  ( Monad m
  ) =>
  AST.Rule ->
  m H.Q.Length ->
  m H.Q.Length ->
  m H.Q.Length ->
  m H.Inter.B.Box.Rule
evalRule (AST.Rule dims) defaultW defaultH defaultD =
  undefined

-- H.Inter.B.Box.Rule
--   <$> ( H.Inter.B.Box.Box ()
--           <$> maybe defaultW evalLength width
--           <*> maybe defaultH evalLength height
--           <*> maybe defaultD evalLength depth
--       )

evalVModeRule ::
  ( Monad m
  ) =>
  AST.Rule ->
  m H.Inter.B.Box.Rule
evalVModeRule rule =
  -- ruleToElem rule defaultWidth defaultHeight defaultDepth
  undefined

-- where
--   defaultWidth = use $ typed @Config % to (lookupLengthParameter HP.HSize)
--   defaultHeight = pure $ toScaledPointApprox (0.4 :: Rational) Point
--   defaultDepth = pure 0

evalHModeRule ::
  Monad m =>
  AST.Rule ->
  m H.Inter.B.Box.Rule
evalHModeRule rule =
  undefined

-- ruleToElem rule defaultWidth defaultHeight defaultDepth
-- where
--   defaultWidth = pure (toScaledPointApprox (0.4 :: Rational) Point)
--   defaultHeight = pure (toScaledPointApprox (10 :: Int) Point)
--   defaultDepth = pure 0

evalChar :: (MonadError e m, AsType EvaluationError e) => AST.CharCodeRef -> m H.Codes.CharCode
evalChar = \case
  AST.CharRef c -> pure c
  AST.CharTokenRef c -> noteRange c
  AST.CharCodeNrRef n -> evalInt n >>= noteRange
  where
    noteRange x =
      note (injectTyped CharCodeNotInRange) (H.Codes.fromHexInt x)
