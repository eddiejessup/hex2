module Hex.Stage.Evaluate.Impl.Quantity where

import Hex.Common.Codes qualified as Codes
import Hex.Common.Quantity qualified as Q
import Hex.Stage.Interpret.Build.Box.Elem qualified as H.Inter.B.Box
import Hex.Stage.Evaluate.Impl.Common (EvaluationError(..))
import Hex.Stage.Parse.Interface.AST.Command qualified as P
import Hex.Stage.Parse.Interface.AST.Quantity qualified as P
import Hexlude

evalSignedValue ::
  Functor m =>
  (a -> m b) ->
  P.Signed a ->
  m (Q.Signed b)
evalSignedValue evalU (P.Signed signs u) = do
  Q.Signed (evalSigns signs) <$> evalU u
  where
    evalSigns :: [Q.Sign] -> Q.Sign
    evalSigns = mconcat

evalInt :: Monad m => P.HexInt -> m Q.HexInt
evalInt n = do
  evalSignedValue evalUnsignedInt n.unInt >>= \case
    Q.Signed Q.Positive x -> pure x
    Q.Signed Q.Negative x -> pure $ -x

evalUnsignedInt :: Monad m => P.UnsignedInt -> m Q.HexInt
evalUnsignedInt = \case
  P.NormalUnsignedInt normalInt -> evalNormalInt normalInt
  P.CoercedUnsignedInt coercedInt -> evalCoercedInt coercedInt

evalNormalInt :: Monad m => P.NormalInt -> m Q.HexInt
evalNormalInt = \case
  P.IntConstant intConstantDigits -> pure $ evalIntConstantDigits intConstantDigits
  P.CharLikeCode word8 -> pure $ Q.HexInt $ word8ToInt word8
  P.InternalInt internalInt -> evalInternalInt internalInt

evalIntConstantDigits :: P.IntConstantDigits -> Q.HexInt
evalIntConstantDigits x =
  let baseInt = case x.intBase of
        P.Base8 -> 8
        P.Base10 -> 10
        P.Base16 -> 16
   in digitsToHexInt baseInt (word8ToInt <$> x.digits)

word8ToInt :: Word8 -> Int
word8ToInt = fromEnum

evalCoercedInt :: p -> a
evalCoercedInt _x = notImplemented "evalCoercedInt"

evalInternalInt :: p -> a
evalInternalInt _x = notImplemented "evalInternalInt"

-- | Convert a list of digits in some base, into the integer they represent in
-- that base.
digitsToHexInt :: Int -> [Int] -> Q.HexInt
digitsToHexInt base digs =
  Q.HexInt $ foldl' (\a b -> a * base + b) 0 digs

evalLength :: P.Length -> m Q.Length
evalLength = notImplemented "evalLength"

evalGlue :: P.Glue -> m Q.Glue
evalGlue = notImplemented "evalGlue"

evalRule ::
  P.Rule ->
  m Q.Length ->
  m Q.Length ->
  m Q.Length ->
  m H.Inter.B.Box.Rule
evalRule (P.Rule _dims) _defaultW _defaultH _defaultD =
  notImplemented "evalRule"

-- H.Inter.B.Box.Rule
--   <$> ( H.Inter.B.Box.Box ()
--           <$> maybe defaultW evalLength width
--           <*> maybe defaultH evalLength height
--           <*> maybe defaultD evalLength depth
--       )

evalVModeRule ::
  P.Rule ->
  m H.Inter.B.Box.Rule
evalVModeRule _rule =
  -- ruleToElem rule defaultWidth defaultHeight defaultDepth
  notImplemented "evalVModeRule"

-- where
--   defaultWidth = use $ typed @Config % to (lookupLengthParameter HP.HSize)
--   defaultHeight = pure $ toScaledPointApprox (0.4 :: Rational) Point
--   defaultDepth = pure 0

evalHModeRule ::
  P.Rule ->
  m H.Inter.B.Box.Rule
evalHModeRule _rule =
  notImplemented "evalHModeRule"

-- ruleToElem rule defaultWidth defaultHeight defaultDepth
-- where
--   defaultWidth = pure (toScaledPointApprox (0.4 :: Rational) Point)
--   defaultHeight = pure (toScaledPointApprox (10 :: Int) Point)
--   defaultDepth = pure 0

evalChar :: (MonadError e m, AsType EvaluationError e) => P.CharCodeRef -> m Codes.CharCode
evalChar = \case
  P.CharRef c -> pure c
  P.CharTokenRef c -> noteRange c
  P.CharCodeNrRef n -> evalCharCodeInt n

evalCharCodeInt ::
  (MonadError e m, AsType EvaluationError e) =>
  P.CharCodeInt ->
  m Codes.CharCode
evalCharCodeInt n =
  evalInt n.unCharCodeInt >>= noteRange

noteRange :: (Codes.HexCode a, MonadError e m, AsType EvaluationError e) => Q.HexInt -> m a
noteRange x =
  note
    (injectTyped ValueNotInRange)
    (Codes.fromHexInt x)
