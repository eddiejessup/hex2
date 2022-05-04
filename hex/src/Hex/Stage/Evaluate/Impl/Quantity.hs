module Hex.Stage.Evaluate.Impl.Quantity where

import Hex.Common.Codes qualified as Code
import Hex.Common.Codes qualified as Codes
import Hex.Common.HexState.Interface (MonadHexState (getSpecialIntParameter))
import Hex.Common.HexState.Interface qualified as HSt
import Hex.Common.HexState.Interface.Resolve.PrimitiveToken qualified as PT
import Hex.Common.Quantity qualified as Q
import Hex.Stage.Evaluate.Impl.Common (EvaluationError (..))
import Hex.Stage.Evaluate.Impl.Common qualified as Eval
import Hex.Stage.Evaluate.Interface.AST.Quantity qualified as E
import Hex.Stage.Interpret.Build.Box.Elem qualified as H.Inter.B.Box
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

evalInt :: (MonadError e m, AsType Eval.EvaluationError e, MonadHexState m) => P.HexInt -> m Q.HexInt
evalInt n = do
  evalSignedValue evalUnsignedInt n.unInt >>= \case
    Q.Signed Q.Positive x -> pure x
    Q.Signed Q.Negative x -> pure $ -x

evalUnsignedInt :: (MonadError e m, AsType Eval.EvaluationError e, MonadHexState m) => P.UnsignedInt -> m Q.HexInt
evalUnsignedInt = \case
  P.NormalUnsignedInt normalInt -> evalNormalInt normalInt
  P.CoercedUnsignedInt coercedInt -> evalCoercedInt coercedInt

evalNormalInt :: (MonadError e m, AsType Eval.EvaluationError e, MonadHexState m) => P.NormalInt -> m Q.HexInt
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

evalInternalInt :: (MonadError e m, AsType Eval.EvaluationError e, MonadHexState m) => P.InternalInt -> m Q.HexInt
evalInternalInt = \case
  P.InternalIntVariable v -> evalInternalIntVariable v
  P.InternalSpecialIntParameter v -> getSpecialIntParameter v
  P.InternalCodeTableRef v -> evalCodeTableRefAsTarget v
  P.InternalCharToken n -> pure n
  P.InternalMathCharToken n -> pure n
  P.InternalFontCharRef v -> evalInternalFontCharRef v
  P.LastPenalty -> panic "Not implemented: evaluate LastPenalty"
  P.ParShape -> panic "Not implemented: evaluate ParShape"
  P.InputLineNr -> panic "Not implemented: evaluate InputLineNr"
  P.Badness -> panic "Not implemented: evaluate Badness"

-- | Evaluate the code-table-ref, but only as far as the raw reference. Don't
-- look up the actual value in the reference.
evalCodeTableRefAsRef :: (MonadError e m, AsType Eval.EvaluationError e, MonadHexState m) => P.CodeTableRef -> m E.CodeTableRef
evalCodeTableRefAsRef codeTableRef =
  E.CodeTableRef
    <$> pure codeTableRef.codeType
    <*> evalCharCodeInt codeTableRef.codeIndex

-- | Evaluate the code-table-ref, in the sense of looking up the referred value.
evalCodeTableRefAsTarget :: (MonadError e m, AsType Eval.EvaluationError e, MonadHexState m) => P.CodeTableRef -> m Q.HexInt
evalCodeTableRefAsTarget codeTableRef = do
  eCodeTableRef <- evalCodeTableRefAsRef codeTableRef
  case eCodeTableRef.codeTableType of
    PT.CategoryCodeType -> HSt.getCategory eCodeTableRef.codeTableChar <&> Code.toHexInt
    PT.MathCodeType -> HSt.getMathCode eCodeTableRef.codeTableChar <&> Code.toHexInt
    PT.ChangeCaseCodeType letterCase -> HSt.getChangeCaseCode letterCase eCodeTableRef.codeTableChar <&> Code.toHexInt
    PT.SpaceFactorCodeType -> HSt.getCategory eCodeTableRef.codeTableChar <&> Code.toHexInt
    PT.DelimiterCodeType -> HSt.getCategory eCodeTableRef.codeTableChar <&> Code.toHexInt

evalInternalIntVariable :: (MonadError e m, AsType Eval.EvaluationError e, MonadHexState m) => P.QuantVariableAST 'PT.IntQuantity -> m Q.HexInt
evalInternalIntVariable = notImplemented "evalInternalIntVariable"

evalInternalFontCharRef :: (MonadError e m, AsType Eval.EvaluationError e, MonadHexState m) => _ -> m Q.HexInt
evalInternalFontCharRef = notImplemented "evalInternalFontCharRef"

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

evalChar :: (MonadError e m, AsType EvaluationError e, MonadHexState m) => P.CharCodeRef -> m Codes.CharCode
evalChar = \case
  P.CharRef c -> pure c
  P.CharTokenRef c -> noteRange c
  P.CharCodeNrRef n -> evalCharCodeInt n

evalCharCodeInt ::
  (MonadError e m, AsType EvaluationError e, MonadHexState m) =>
  P.CharCodeInt ->
  m Codes.CharCode
evalCharCodeInt n =
  evalInt n.unCharCodeInt >>= noteRange

noteRange :: (Codes.HexCode a, MonadError e m, AsType EvaluationError e) => Q.HexInt -> m a
noteRange x =
  note
    (injectTyped ValueNotInRange)
    (Codes.fromHexInt x)
