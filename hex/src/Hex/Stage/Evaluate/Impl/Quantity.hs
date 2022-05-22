module Hex.Stage.Evaluate.Impl.Quantity where

import Data.List qualified as List
import Data.Ratio qualified as Ratio
import Hex.Common.Codes qualified as Code
import Hex.Common.HexState.Interface (MonadHexState)
import Hex.Common.HexState.Interface qualified as HSt
import Hex.Common.HexState.Interface.Parameter qualified as HSt.Param
import Hex.Common.HexState.Interface.Register qualified as HSt.Reg
import Hex.Common.HexState.Interface.Register qualified as Hst.Reg
import Hex.Common.HexState.Interface.TokenList (BalancedText)
import Hex.Common.HexState.Interface.Variable qualified as HSt.Var
import Hex.Common.Quantity qualified as Q
import Hex.Stage.Evaluate.Impl.Common (EvaluationError (..))
import Hex.Stage.Evaluate.Impl.Common qualified as Eval
import Hex.Stage.Evaluate.Interface.AST.Quantity qualified as E
import Hex.Stage.Interpret.Build.Box.Elem qualified as H.Inter.B.Box
import Hex.Stage.Parse.Interface.AST.Command qualified as P
import Hex.Stage.Parse.Interface.AST.Quantity qualified as P
import Hexlude

evalSignedValue ::
  (Monad m, Group b) =>
  (a -> m b) ->
  P.Signed a ->
  m b
evalSignedValue evalU (P.Signed signs u) = do
  let eSign = evalSigns signs
  eU <- evalU u
  case eSign of
    Q.Positive -> pure eU
    Q.Negative -> pure $ invert eU
  where
    evalSigns :: [Q.Sign] -> Q.Sign
    evalSigns = mconcat

evalInt :: (MonadError e m, AsType Eval.EvaluationError e, MonadHexState m) => P.HexInt -> m Q.HexInt
evalInt n = do
  evalSignedValue evalUnsignedInt n.unInt

evalUnsignedInt :: (MonadError e m, AsType Eval.EvaluationError e, MonadHexState m) => P.UnsignedInt -> m Q.HexInt
evalUnsignedInt = \case
  P.NormalUnsignedInt normalInt -> evalNormalInt normalInt
  P.CoercedUnsignedInt coercedInt -> evalCoercedInt coercedInt

evalNormalInt :: (MonadError e m, AsType Eval.EvaluationError e, MonadHexState m) => P.NormalInt -> m Q.HexInt
evalNormalInt = \case
  P.IntConstant intConstantDigits -> pure $ Q.HexInt $ evalIntConstantDigits intConstantDigits
  P.CharLikeCode word8 -> pure $ Q.HexInt $ word8ToInt word8
  P.InternalInt internalInt -> evalInternalInt internalInt

evalIntConstantDigits :: P.IntConstantDigits -> Int
evalIntConstantDigits x =
  let baseInt = case x.intBase of
        P.Base8 -> 8
        P.Base10 -> 10
        P.Base16 -> 16
   in digitsToInt baseInt (word8ToInt <$> x.digits)

word8ToInt :: Word8 -> Int
word8ToInt = fromIntegral

evalCoercedInt :: p -> a
evalCoercedInt _x = notImplemented "evalCoercedInt"

evalInternalInt :: (MonadError e m, AsType Eval.EvaluationError e, MonadHexState m) => P.InternalInt -> m Q.HexInt
evalInternalInt = \case
  P.InternalIntVariable v -> evalQuantVariableAsTarget v
  P.InternalSpecialIntParameter v -> HSt.getSpecialIntParameter v
  P.InternalCodeTableRef v -> evalCodeTableRefAsTarget v
  P.InternalCharToken n -> pure n
  P.InternalMathCharToken n -> pure n
  P.InternalFontSpecialCharRef v -> evalFontSpecialCharRef v
  P.LastPenalty -> notImplemented "evalInternalInt: LastPenalty"
  P.ParShape -> notImplemented "evalInternalInt: ParShape"
  P.InputLineNr -> notImplemented "evalInternalInt: InputLineNr"
  P.Badness -> notImplemented "evalInternalInt: Badness"

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
    Code.CatCodeType -> HSt.getHexCode Code.CCatCodeType eCodeTableRef.codeTableChar <&> Code.toHexInt
    Code.MathCodeType -> HSt.getHexCode Code.CMathCodeType eCodeTableRef.codeTableChar <&> Code.toHexInt
    Code.UpperCaseCodeType -> HSt.getHexCode Code.CUpperCaseCodeType eCodeTableRef.codeTableChar <&> Code.toHexInt
    Code.LowerCaseCodeType -> HSt.getHexCode Code.CLowerCaseCodeType eCodeTableRef.codeTableChar <&> Code.toHexInt
    Code.SpaceFactorCodeType -> HSt.getHexCode Code.CSpaceFactorCodeType eCodeTableRef.codeTableChar <&> Code.toHexInt
    Code.DelimiterCodeType -> HSt.getHexCode Code.CDelimiterCodeType eCodeTableRef.codeTableChar <&> Code.toHexInt

evalQuantVariableAsVariable :: (MonadError e m, AsType Eval.EvaluationError e, HSt.MonadHexState m) => P.QuantVariableAST a -> m (HSt.Var.QuantVariable a)
evalQuantVariableAsVariable = \case
  P.ParamVar intParam -> pure $ HSt.Var.ParamVar intParam
  P.RegisterVar registerLocation -> HSt.Var.RegisterVar <$> evalRegisterLocationAsLocation registerLocation

evalQuantVariableAsTarget ::
  ( MonadError e m,
    AsType Eval.EvaluationError e,
    HSt.MonadHexState m
  ) =>
  P.QuantVariableAST a ->
  m (HSt.Var.QuantVariableTarget a)
evalQuantVariableAsTarget =
  evalQuantVariableAsVariable >=> \case
    HSt.Var.ParamVar p -> HSt.getParameterValue p
    HSt.Var.RegisterVar loc -> HSt.getRegisterValue loc

evalRegisterLocationAsLocation ::
  ( MonadError e m,
    AsType Eval.EvaluationError e,
    HSt.MonadHexState m
  ) =>
  P.QuantRegisterLocation q ->
  m (HSt.Reg.QuantRegisterLocation q)
evalRegisterLocationAsLocation = \case
  P.ExplicitRegisterLocation regType n ->
    Hst.Reg.QuantRegisterLocation regType . Hst.Reg.RegisterLocation <$> evalInt n
  P.InternalRegisterLocation loc ->
    pure loc

evalFontSpecialCharRef :: P.FontSpecialCharRef -> m Q.HexInt
evalFontSpecialCharRef = notImplemented "evalFontSpecialCharRef"

-- | Convert a list of digits in some base, into the integer they represent in
-- that base.
digitsToInt :: Int -> [Int] -> Int
digitsToInt base digs =
  foldl' (\a b -> a * base + b) 0 digs

evalLength :: (MonadError e m, AsType Eval.EvaluationError e, MonadHexState m) => P.Length -> m Q.Length
evalLength len = do
  evalSignedValue evalUnsignedLength len.unLength

evalUnsignedLength :: (MonadError e m, AsType Eval.EvaluationError e, MonadHexState m) => P.UnsignedLength -> m Q.Length
evalUnsignedLength = \case
  P.NormalLengthAsULength normalLength ->
    evalNormalLength normalLength
  P.CoercedLength (P.InternalGlueAsLength internalGlue) ->
    (.gDimen) <$> evalInternalGlue internalGlue

evalNormalLength :: (MonadError e m, AsType Eval.EvaluationError e, MonadHexState m) => P.NormalLength -> m Q.Length
evalNormalLength = \case
  P.LengthSemiConstant factor unit -> do
    eFactor <- evalFactor factor
    eUnit <- evalUnit unit
    pure $ Q.scaleLengthByRational eFactor eUnit
  P.InternalLength internalLength -> evalInternalLength internalLength

evalFactor :: (MonadError e m, AsType Eval.EvaluationError e, MonadHexState m) => P.Factor -> m Rational
evalFactor = \case
  P.NormalIntFactor normalInt -> do
    hexInt <- evalNormalInt normalInt
    pure $ fromIntegral @Int @Rational hexInt.unHexInt
  P.DecimalFractionFactor decimalFraction -> pure $ evalDecimalFraction decimalFraction

evalDecimalFraction :: P.DecimalFraction -> Rational
evalDecimalFraction v =
  let wholeNr = decDigitsToInt v.wholeDigits

      fraction =
        (decDigitsToInt v.fracDigits)
          Ratio.% (10 ^ List.length v.fracDigits)
   in -- Convert the whole number to a rational, and add it to the fraction.

      (fromIntegral @Integer @Rational wholeNr) + fraction
  where
    decDigitsToInt words =
      fromIntegral @Int @Integer $ digitsToInt 10 $ word8ToInt <$> words

evalUnit :: (MonadError e m, AsType Eval.EvaluationError e, MonadHexState m) => P.Unit -> m Q.Length
evalUnit = \case
  P.PhysicalUnit physicalUnitFrame physicalUnit -> do
    eFrame <- evalPhysicalUnitFrame physicalUnitFrame
    let eUnit = Q.inScaledPoint physicalUnit
    pure $ Q.scaleLengthByRational eFrame eUnit
  P.InternalUnit internalUnit -> do
    evalInternalUnit internalUnit

evalPhysicalUnitFrame :: (MonadHexState m) => P.PhysicalUnitFrame -> m Rational
evalPhysicalUnitFrame = \case
  P.MagnifiedFrame ->
    pure 1.0
  P.TrueFrame -> do
    _mag <- HSt.getParameterValue (HSt.Param.IntQuantParam HSt.Param.Mag)
    notImplemented "evalPhysicalUnitFrame: MagnifiedFrame"

evalInternalUnit :: (MonadError e m, AsType Eval.EvaluationError e, MonadHexState m) => P.InternalUnit -> m Q.Length
evalInternalUnit = \case
  P.Em -> notImplemented "evalInternalUnit: Em"
  P.Ex -> notImplemented "evalInternalUnit: Ex"
  P.InternalIntUnit internalInt ->
    Q.lengthFromHexInt <$> evalInternalInt internalInt
  P.InternalLengthUnit internalLength ->
    evalInternalLength internalLength
  P.InternalGlueUnit internalGlue ->
    (.gDimen) <$> evalInternalGlue internalGlue

evalGlue :: (MonadError e m, AsType Eval.EvaluationError e, MonadHexState m) => P.Glue -> m Q.Glue
evalGlue = \case
  P.ExplicitGlue explicitGlueSpec -> evalExplicitGlueSpec explicitGlueSpec
  P.InternalGlue signedInternalGlue -> evalSignedValue evalInternalGlue signedInternalGlue

evalExplicitGlueSpec :: (MonadError e m, AsType Eval.EvaluationError e, MonadHexState m) => P.ExplicitGlueSpec -> m Q.Glue
evalExplicitGlueSpec P.ExplicitGlueSpec {egLength, egStretch, egShrink} = do
  gDimen <- evalLength egLength
  gStretch <- evalMayFlex egStretch
  gShrink <- evalMayFlex egShrink
  pure $ Q.Glue {gDimen, gStretch, gShrink}

evalMayFlex :: (MonadError e m, AsType Eval.EvaluationError e, MonadHexState m) => Maybe P.PureFlex -> m Q.PureFlex
evalMayFlex = \case
  Nothing -> pure Q.zeroFlex
  Just flex -> evalPureFlex flex

evalPureFlex :: (MonadError e m, AsType Eval.EvaluationError e, MonadHexState m) => P.PureFlex -> m Q.PureFlex
evalPureFlex = \case
  P.FinitePureFlex finiteFlexLength -> Q.FinitePureFlex <$> evalLength finiteFlexLength
  P.InfPureFlex infFlexOfOrder -> Q.InfPureFlex <$> evalInfFlexOfOrder infFlexOfOrder

evalInfFlexOfOrder :: (MonadError e m, AsType Eval.EvaluationError e, MonadHexState m) => P.InfFlexOfOrder -> m Q.InfFlexOfOrder
evalInfFlexOfOrder (P.InfFlexOfOrder signedFactor infFlexOrder) = do
  -- Wrap/unwrap via a 'Sum' newtype, to get the 'Sum' instance of 'Group', so
  -- we get the negative of the rational when needed.
  (Sum factorRational) <- evalSignedValue (fmap Sum . evalFactor) signedFactor
  let factorInfLength = Q.fromBigFils factorRational
  pure $ Q.InfFlexOfOrder factorInfLength infFlexOrder

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

evalChar ::
  (MonadError e m, AsType EvaluationError e, MonadHexState m) =>
  P.CharCodeRef ->
  m Code.CharCode
evalChar = \case
  P.CharRef c -> pure c
  P.CharTokenRef c -> noteRange c
  P.CharCodeNrRef n -> evalCharCodeInt n

evalCharCodeInt ::
  (MonadError e m, AsType EvaluationError e, MonadHexState m) =>
  P.CharCodeInt ->
  m Code.CharCode
evalCharCodeInt n =
  evalInt n.unCharCodeInt >>= noteRange

noteRange :: (Code.HexCode a, MonadError e m, AsType EvaluationError e) => Q.HexInt -> m a
noteRange x =
  note
    (injectTyped ValueNotInRange)
    (Code.fromHexInt x)

evalMathLength :: (MonadError e m, AsType EvaluationError e, MonadHexState m) => P.MathLength -> m Q.MathLength
evalMathLength mathLength = evalSignedValue (evalUnsignedMathLength) (mathLength.unMathLength)

evalUnsignedMathLength :: (MonadError e m, AsType EvaluationError e, MonadHexState m) => P.UnsignedMathLength -> m Q.MathLength
evalUnsignedMathLength = \case
  P.NormalMathLengthAsUMathLength normalMathLength -> evalNormalMathLength normalMathLength
  P.CoercedMathLength coercedMathLength -> evalCoercedMathLength coercedMathLength

evalNormalMathLength :: (MonadError e m, AsType EvaluationError e, MonadHexState m) => P.NormalMathLength -> m Q.MathLength
evalNormalMathLength = \case
  P.MathLengthSemiConstant factor mathUnit -> do
    eFactor <- evalFactor factor
    eMathUnit <- evalMathUnit mathUnit
    pure $ Q.scaleMathLengthByRational eFactor eMathUnit

evalMathUnit :: (MonadError e m, AsType EvaluationError e, MonadHexState m) => P.MathUnit -> m Q.MathLength
evalMathUnit = \case
  P.Mu ->
    pure Q.muLength
  P.InternalMathGlueAsUnit internalMathGlue ->
    (.mgDimen) <$> evalInternalMathGlue internalMathGlue

evalCoercedMathLength :: (MonadError e m, AsType EvaluationError e, MonadHexState m) => P.CoercedMathLength -> m Q.MathLength
evalCoercedMathLength = \case
  P.InternalMathGlueAsMathLength internalMathGlue ->
    (.mgDimen) <$> evalInternalMathGlue internalMathGlue

evalMathGlue :: (MonadError e m, AsType EvaluationError e, MonadHexState m) => P.MathGlue -> m Q.MathGlue
evalMathGlue = \case
  P.ExplicitMathGlue mathLength mayMathStretch mayMathShrink -> do
    Q.MathGlue <$> evalMathLength mathLength <*> evalMayMathFlex mayMathStretch <*> evalMayMathFlex mayMathShrink
  P.InternalMathGlue signedInternalMathGlue -> do
    evalSignedValue evalInternalMathGlue signedInternalMathGlue

evalMayMathFlex :: (MonadError e m, AsType Eval.EvaluationError e, MonadHexState m) => Maybe P.PureMathFlex -> m Q.PureMathFlex
evalMayMathFlex = \case
  Nothing -> pure Q.zeroMathFlex
  Just flex -> evalPureMathFlex flex

evalPureMathFlex :: (MonadError e m, AsType Eval.EvaluationError e, MonadHexState m) => P.PureMathFlex -> m Q.PureMathFlex
evalPureMathFlex = \case
  P.FinitePureMathFlex finiteFlexMathLength -> Q.FinitePureMathFlex <$> evalMathLength finiteFlexMathLength
  P.InfPureMathFlex infFlexOfOrder -> Q.InfPureMathFlex <$> evalInfFlexOfOrder infFlexOfOrder

evalTokenListAssignmentTarget :: P.TokenListAssignmentTarget -> m BalancedText
evalTokenListAssignmentTarget = notImplemented "evalTokenListAssignmentTarget"

evalInternalQuantity ::
  (MonadError e m, AsType EvaluationError e, MonadHexState m) =>
  P.InternalQuantity ->
  m E.InternalQuantity
evalInternalQuantity = \case
  P.InternalIntQuantity internalInt ->
    E.InternalIntQuantity <$> evalInternalInt internalInt
  P.InternalLengthQuantity internalLength ->
    E.InternalLengthQuantity <$> evalInternalLength internalLength
  P.InternalGlueQuantity internalGlue ->
    E.InternalGlueQuantity <$> evalInternalGlue internalGlue
  P.InternalMathGlueQuantity internalMathGlue ->
    E.InternalMathGlueQuantity <$> evalInternalMathGlue internalMathGlue
  P.FontQuantity _fontRef ->
    notImplemented "evalInternalQuantity: FontQuantity"
  P.TokenListVariableQuantity _tokenListVariable ->
    notImplemented "evalInternalQuantity: TokenListVariableQuantity"

evalInternalLength :: (MonadError e m, AsType Eval.EvaluationError e, MonadHexState m) => P.InternalLength -> m Q.Length
evalInternalLength = \case
  P.InternalLengthVariable lengthVariable ->
    evalQuantVariableAsTarget lengthVariable
  P.InternalSpecialLengthParameter specialLengthParameter ->
    HSt.getSpecialLengthParameter specialLengthParameter
  P.InternalFontDimensionRef _fontDimensionRef ->
    notImplemented "evalInternalLength: InternalFontDimensionRef"
  P.InternalBoxDimensionRef _boxDimensionRef ->
    notImplemented "evalInternalLength: InternalBoxDimensionRef"
  P.LastKern ->
    notImplemented "evalInternalLength: LastKern"

evalInternalGlue :: (MonadError e m, AsType Eval.EvaluationError e, MonadHexState m) => P.InternalGlue -> m Q.Glue
evalInternalGlue = \case
  P.InternalGlueVariable glueVariable ->
    evalQuantVariableAsTarget glueVariable
  P.LastGlue ->
    notImplemented "evalInternalGlue: LastGlue"

evalInternalMathGlue :: (MonadError e m, AsType Eval.EvaluationError e, MonadHexState m) => P.InternalMathGlue -> m Q.MathGlue
evalInternalMathGlue = \case
  P.InternalMathGlueVariable mathGlueVariable ->
    evalQuantVariableAsTarget mathGlueVariable
  P.LastMathGlue ->
    notImplemented "evalInternalGlue: LastMathGlue"
