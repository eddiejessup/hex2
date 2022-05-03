module Hex.Stage.Evaluate.Impl.Eval where

import Hex.Common.Codes qualified as H.Codes
import Hex.Common.Quantity qualified as H.Q
import Hex.Stage.Interpret.Build.Box.Elem qualified as H.Inter.B.Box
import Hex.Stage.Parse.Interface.AST.Command qualified as AST
import Hex.Stage.Parse.Interface.AST.Common qualified as AST
import Hexlude

data EvaluationError
  = CharCodeNotInRange
  deriving stock (Show, Generic)

evalSignedValue ::
  Functor m =>
  (a -> m b) ->
  AST.Signed a ->
  m (H.Q.Signed b)
evalSignedValue evalU (AST.Signed signs u) = do
  H.Q.Signed (evalSigns signs) <$> evalU u
  where
    evalSigns :: [H.Q.Sign] -> H.Q.Sign
    evalSigns = mconcat

evalInt :: Monad m => AST.HexInt -> m H.Q.HexInt
evalInt n = do
  evalSignedValue evalUnsignedInt n.unInt >>= \case
    H.Q.Signed H.Q.Positive x -> pure x
    H.Q.Signed H.Q.Negative x -> pure $ -x

evalUnsignedInt :: Monad m => AST.UnsignedInt -> m H.Q.HexInt
evalUnsignedInt = \case
  AST.NormalUnsignedInt normalInt -> evalNormalInt normalInt
  AST.CoercedUnsignedInt coercedInt -> evalCoercedInt coercedInt

evalNormalInt :: Monad m => AST.NormalInt -> m H.Q.HexInt
evalNormalInt = \case
  AST.IntConstant intConstantDigits -> pure $ evalIntConstantDigits intConstantDigits
  AST.CharLikeCode word8 -> pure $ H.Q.HexInt $ word8ToInt word8
  AST.InternalInt internalInt -> evalInternalInt internalInt

evalIntConstantDigits :: AST.IntConstantDigits -> H.Q.HexInt
evalIntConstantDigits x =
  let baseInt = case x.intBase of
        AST.Base8 -> 8
        AST.Base10 -> 10
        AST.Base16 -> 16
   in digitsToHexInt baseInt (word8ToInt <$> x.digits)

word8ToInt :: Word8 -> Int
word8ToInt = fromEnum

evalCoercedInt :: p -> a
evalCoercedInt _x = panic "Not implemented: evalCoercedInt"

evalInternalInt :: p -> a
evalInternalInt _x = panic "Not implemented: evalInternalInt"

-- | Convert a list of digits in some base, into the integer they represent in
-- that base.
digitsToHexInt :: Int -> [Int] -> H.Q.HexInt
digitsToHexInt base digs =
  H.Q.HexInt $ foldl' (\a b -> a * base + b) 0 digs

evalLength :: AST.Length -> m H.Q.Length
evalLength = panic "Not implemented: evalLength"

evalGlue :: AST.Glue -> m H.Q.Glue
evalGlue = panic "Not implemented: evalGlue"

evalRule ::
  AST.Rule ->
  m H.Q.Length ->
  m H.Q.Length ->
  m H.Q.Length ->
  m H.Inter.B.Box.Rule
evalRule (AST.Rule _dims) _defaultW _defaultH _defaultD =
  panic "Not implemented: evalRule"

-- H.Inter.B.Box.Rule
--   <$> ( H.Inter.B.Box.Box ()
--           <$> maybe defaultW evalLength width
--           <*> maybe defaultH evalLength height
--           <*> maybe defaultD evalLength depth
--       )

evalVModeRule ::
  AST.Rule ->
  m H.Inter.B.Box.Rule
evalVModeRule _rule =
  -- ruleToElem rule defaultWidth defaultHeight defaultDepth
  panic "Not implemented: evalVModeRule"

-- where
--   defaultWidth = use $ typed @Config % to (lookupLengthParameter HP.HSize)
--   defaultHeight = pure $ toScaledPointApprox (0.4 :: Rational) Point
--   defaultDepth = pure 0

evalHModeRule ::
  AST.Rule ->
  m H.Inter.B.Box.Rule
evalHModeRule _rule =
  panic "Not implemented: evalHModeRule"

-- ruleToElem rule defaultWidth defaultHeight defaultDepth
-- where
--   defaultWidth = pure (toScaledPointApprox (0.4 :: Rational) Point)
--   defaultHeight = pure (toScaledPointApprox (10 :: Int) Point)
--   defaultDepth = pure 0

evalChar :: (MonadError e m, AsType EvaluationError e) => AST.CharCodeRef -> m H.Codes.CharCode
evalChar = \case
  AST.CharRef c -> pure c
  AST.CharTokenRef c -> noteRange c
  AST.CharCodeNrRef n -> evalCharCodeInt n

evalCharCodeInt ::
  (MonadError e m, AsType EvaluationError e) =>
  AST.CharCodeInt ->
  m H.Codes.CharCode
evalCharCodeInt n =
  evalInt n.unCharCodeInt >>= noteRange

noteRange :: (MonadError e m, AsType EvaluationError e, H.Codes.HexCode a) => H.Q.HexInt -> m a
noteRange x =
  note
    (injectTyped CharCodeNotInRange)
    (H.Codes.fromHexInt x)
