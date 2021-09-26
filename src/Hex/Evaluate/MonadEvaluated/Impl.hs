module Hex.Evaluate.MonadEvaluated.Impl where

import Hex.Codes qualified as H.Codes
import Hex.Interpret.Build.Box.Elem qualified as H.Inter.B.Box
import Hex.Parse.Syntax.Quantity qualified as H.Par.Syn
import Hex.Parse.Syntax.Command qualified as H.Par.Syn
import Hex.Syntax.Command qualified as H.Syn
import Hex.Syntax.Common qualified as H.Syn
import Hex.Quantity qualified as H.Q
import Hexlude

data EvaluationError
  = CharCodeNotInRange
  deriving stock (Show, Generic)

-- evalASTInt :: Monad m => H.Par.Syn.HexInt -> m H.Q.HexInt
-- evalASTInt = undefined

-- evalASTLength :: Monad m => H.Par.Syn.Length -> m H.Q.Length
-- evalASTLength = undefined

-- evalASTGlue :: Monad m => H.Par.Syn.Glue -> m H.Q.Glue
-- evalASTGlue = undefined

-- evalASTRule ::
--   ( Monad m
--   ) =>
--   H.Syn.HexPassRule 'H.Syn.Parsed ->
--   m H.Q.Length ->
--   m H.Q.Length ->
--   m H.Q.Length ->
--   m (H.Syn.HexPassRule 'H.Syn.Evaluated)
-- evalASTRule (H.Par.Syn.Rule dims) defaultW defaultH defaultD =
--   undefined

-- -- H.Inter.B.Box.Rule
-- --   <$> ( H.Inter.B.Box.Box ()
-- --           <$> maybe defaultW evalASTLength width
-- --           <*> maybe defaultH evalASTLength height
-- --           <*> maybe defaultD evalASTLength depth
-- --       )

-- evalASTVModeRule ::
--   ( Monad m
--   ) =>
--   H.Syn.HexPassRule 'H.Syn.Parsed ->
--   m H.Inter.B.Box.Rule
-- evalASTVModeRule rule =
--   -- ruleToElem rule defaultWidth defaultHeight defaultDepth
--   undefined

-- -- where
-- --   defaultWidth = use $ typed @Config % to (lookupLengthParameter HP.HSize)
-- --   defaultHeight = pure $ toScaledPointApprox (0.4 :: Rational) Point
-- --   defaultDepth = pure 0

-- evalASTHModeRule ::
--   Monad m =>
--   H.Syn.HexPassRule 'H.Syn.Parsed ->
--   m H.Inter.B.Box.Rule
-- evalASTHModeRule rule =
--   undefined

-- -- ruleToElem rule defaultWidth defaultHeight defaultDepth
-- -- where
-- --   defaultWidth = pure (toScaledPointApprox (0.4 :: Rational) Point)
-- --   defaultHeight = pure (toScaledPointApprox (10 :: Int) Point)
-- --   defaultDepth = pure 0

-- evalASTChar :: (MonadError e m, AsType EvaluationError e) => H.Syn.CharCodeRef 'H.Syn.Parsed -> m H.Codes.CharCode
-- evalASTChar = \case
--   H.Syn.CharRef c -> pure c
--   H.Syn.CharTokenRef c -> noteRange c
--   H.Syn.CharCodeNrRef n -> evalASTInt n >>= noteRange
--   where
--     noteRange x =
--       note (injectTyped CharCodeNotInRange) (H.Codes.fromHexInt x)
