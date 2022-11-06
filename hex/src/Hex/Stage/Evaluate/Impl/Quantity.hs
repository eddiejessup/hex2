module Hex.Stage.Evaluate.Impl.Quantity where

import Data.List qualified as List
import Data.Ratio qualified as Ratio
import Data.Sequence qualified as Seq
import GHC.Num qualified as Num
import Hex.Common.Box qualified as Box
import Hex.Common.Codes qualified as Code
import Hex.Common.Font qualified as Font
import Hex.Common.HexState.Interface (EHexState)
import Hex.Common.HexState.Interface qualified as HSt
import Hex.Common.HexState.Interface.Font qualified as HSt.Font
import Hex.Common.HexState.Interface.Parameter qualified as HSt.Param
import Hex.Common.HexState.Interface.Register qualified as HSt.Reg
import Hex.Common.HexState.Interface.TokenList qualified as HSt.TL
import Hex.Common.HexState.Interface.Variable qualified as HSt.Var
import Hex.Common.Quantity qualified as Q
import Hex.Stage.Evaluate.Impl.Common (EvaluationError (..))
import Hex.Stage.Evaluate.Impl.Common qualified as Eval
import Hex.Stage.Evaluate.Interface.AST.Quantity qualified as E
import Hex.Stage.Parse.Interface.AST.Command qualified as P
import Hex.Stage.Parse.Interface.AST.Quantity qualified as P
import Hexlude

evalSignedValue ::
  Group b =>
  (a -> Eff es b) ->
  P.Signed a ->
  Eff es b
evalSignedValue evalU (P.Signed signs u) = do
  let eSign = evalSigns signs
  eU <- evalU u
  case eSign of
    Q.Positive -> pure eU
    Q.Negative -> pure $ invert eU
  where
    evalSigns :: [Q.Sign] -> Q.Sign
    evalSigns = mconcat

evalIntToFourBitUnsigned ::
  (Error Eval.EvaluationError :> es, EHexState :> es) =>
  P.HexInt ->
  Eff es Q.FourBitInt
evalIntToFourBitUnsigned n = do
  Q.newFourBitInt <$> evalInt n >>= \case
    Nothing ->
      throwError ValueNotInRange
    Just fourN ->
      pure fourN

evalInt :: (Error Eval.EvaluationError :> es, EHexState :> es) => P.HexInt -> Eff es Q.HexInt
evalInt n = do
  evalSignedValue evalUnsignedInt n.unInt

evalUnsignedInt :: (Error Eval.EvaluationError :> es, EHexState :> es) => P.UnsignedInt -> Eff es Q.HexInt
evalUnsignedInt = \case
  P.NormalUnsignedInt normalInt -> evalNormalInt normalInt
  P.CoercedUnsignedInt coercedInt -> evalCoercedInt coercedInt

evalNormalInt :: (Error Eval.EvaluationError :> es, EHexState :> es) => P.NormalInt -> Eff es Q.HexInt
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

evalCoercedInt :: (Error Eval.EvaluationError :> es, EHexState :> es) => P.CoercedInt -> Eff es Q.HexInt
evalCoercedInt = \case
  P.InternalLengthAsInt internalLength ->
    Q.lengthAsInt <$> evalInternalLength internalLength
  P.InternalGlueAsInt internalGlue ->
    (Q.lengthAsInt . Q.glueAsLength) <$> evalInternalGlue internalGlue

evalInternalInt :: (Error Eval.EvaluationError :> es, EHexState :> es) => P.InternalInt -> Eff es Q.HexInt
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
evalCodeTableRefAsRef :: (Error Eval.EvaluationError :> es, EHexState :> es) => P.CodeTableRef -> Eff es E.CodeTableRef
evalCodeTableRefAsRef codeTableRef =
  E.CodeTableRef
    <$> pure codeTableRef.codeType
    <*> evalCharCodeInt codeTableRef.codeIndex

-- | Evaluate the code-table-ref, in the sense of looking up the referred value.
evalCodeTableRefAsTarget :: (Error Eval.EvaluationError :> es, EHexState :> es) => P.CodeTableRef -> Eff es Q.HexInt
evalCodeTableRefAsTarget codeTableRef = do
  eCodeTableRef <- evalCodeTableRefAsRef codeTableRef
  case eCodeTableRef.codeTableType of
    Code.CatCodeType -> HSt.getHexCode Code.CCatCodeType eCodeTableRef.codeTableChar <&> Code.toHexInt
    Code.MathCodeType -> HSt.getHexCode Code.CMathCodeType eCodeTableRef.codeTableChar <&> Code.toHexInt
    Code.UpperCaseCodeType -> HSt.getHexCode Code.CUpperCaseCodeType eCodeTableRef.codeTableChar <&> Code.toHexInt
    Code.LowerCaseCodeType -> HSt.getHexCode Code.CLowerCaseCodeType eCodeTableRef.codeTableChar <&> Code.toHexInt
    Code.SpaceFactorCodeType -> HSt.getHexCode Code.CSpaceFactorCodeType eCodeTableRef.codeTableChar <&> Code.toHexInt
    Code.DelimiterCodeType -> HSt.getHexCode Code.CDelimiterCodeType eCodeTableRef.codeTableChar <&> Code.toHexInt

evalQuantVariableAsVariable :: (Error Eval.EvaluationError :> es, EHexState :> es) => P.QuantVariableAST a -> Eff es (HSt.Var.QuantVariable a)
evalQuantVariableAsVariable = \case
  P.ParamVar intParam -> pure $ HSt.Var.ParamVar intParam
  P.RegisterVar registerLocation -> HSt.Var.RegisterVar <$> evalRegisterLocationAsLocation registerLocation

evalQuantVariableAsTarget ::
  (Error Eval.EvaluationError :> es, EHexState :> es) =>
  P.QuantVariableAST a ->
  Eff es (HSt.Var.QuantVariableTarget a)
evalQuantVariableAsTarget =
  evalQuantVariableAsVariable >=> \case
    HSt.Var.ParamVar p -> HSt.getParameterValue p
    HSt.Var.RegisterVar loc -> HSt.getRegisterValue loc

evalRegisterLocationAsLocation ::
  (Error Eval.EvaluationError :> es, EHexState :> es) =>
  P.QuantRegisterLocation q ->
  Eff es (HSt.Reg.QuantRegisterLocation q)
evalRegisterLocationAsLocation = \case
  P.ExplicitQuantRegisterLocation regType loc ->
    HSt.Reg.QuantRegisterLocation regType <$> evalExplicitRegisterLocation loc
  P.InternalQuantRegisterLocation loc ->
    pure loc

evalExplicitRegisterLocation :: (Error Eval.EvaluationError :> es, EHexState :> es) => P.ExplicitRegisterLocation -> Eff es HSt.Reg.RegisterLocation
evalExplicitRegisterLocation explicitRegisterLocation = HSt.Reg.RegisterLocation <$> evalInt explicitRegisterLocation.unExplicitRegisterLocation

evalFontSpecialCharRef :: P.FontSpecialCharRef -> Eff es Q.HexInt
evalFontSpecialCharRef = notImplemented "evalFontSpecialCharRef"

-- | Convert a list of digits in some base, into the integer they represent in
-- that base.
digitsToInt :: Int -> [Int] -> Int
digitsToInt base digs =
  foldl' (\a b -> a Num.* base Num.+ b) 0 digs

evalLength :: (Error Eval.EvaluationError :> es, EHexState :> es) => P.Length -> Eff es Q.Length
evalLength len =
  evalSignedValue evalUnsignedLength len.unLength

evalUnsignedLength :: (Error Eval.EvaluationError :> es, EHexState :> es) => P.UnsignedLength -> Eff es Q.Length
evalUnsignedLength = \case
  P.NormalLengthAsULength normalLength ->
    evalNormalLength normalLength
  P.CoercedLength (P.InternalGlueAsLength internalGlue) ->
    (.gDimen) <$> evalInternalGlue internalGlue

evalNormalLength :: (Error Eval.EvaluationError :> es, EHexState :> es) => P.NormalLength -> Eff es Q.Length
evalNormalLength = \case
  P.LengthSemiConstant factor unit -> do
    eFactor <- evalFactor factor
    eUnit <- evalUnit unit
    pure $ Q.scaleLengthByRational eFactor eUnit
  P.InternalLength internalLength -> evalInternalLength internalLength

evalFactor :: (Error Eval.EvaluationError :> es, EHexState :> es) => P.Factor -> Eff es Rational
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

      (fromIntegral @Integer @Rational wholeNr) Num.+ fraction
  where
    decDigitsToInt words =
      fromIntegral @Int @Integer $ digitsToInt 10 $ word8ToInt <$> words

evalUnit :: (Error Eval.EvaluationError :> es, EHexState :> es) => P.Unit -> Eff es Q.Length
evalUnit = \case
  P.PhysicalUnit physicalUnitFrame physicalUnit -> do
    eFrame <- evalPhysicalUnitFrame physicalUnitFrame
    let eUnit = Q.inScaledPoint physicalUnit
    pure $ Q.scaleLengthByRational eFrame eUnit
  P.InternalUnit internalUnit -> do
    evalInternalUnit internalUnit

evalPhysicalUnitFrame :: EHexState :> es => P.PhysicalUnitFrame -> Eff es Rational
evalPhysicalUnitFrame = \case
  P.MagnifiedFrame ->
    pure 1.0
  P.TrueFrame -> do
    mag <- HSt.getParameterValue (HSt.Param.IntQuantParam HSt.Param.Mag)
    pure $ recip $ Q.inThousands mag

evalInternalUnit :: (Error Eval.EvaluationError :> es, EHexState :> es) => P.InternalUnit -> Eff es Q.Length
evalInternalUnit = \case
  P.Em -> notImplemented "evalInternalUnit: Em"
  P.Ex -> notImplemented "evalInternalUnit: Ex"
  P.InternalIntUnit internalInt ->
    Q.lengthFromInt <$> evalInternalInt internalInt
  P.InternalLengthUnit internalLength ->
    evalInternalLength internalLength
  P.InternalGlueUnit internalGlue ->
    (.gDimen) <$> evalInternalGlue internalGlue

evalGlue :: (Error Eval.EvaluationError :> es, EHexState :> es) => P.Glue -> Eff es Q.Glue
evalGlue = \case
  P.ExplicitGlue explicitGlueSpec -> evalExplicitGlueSpec explicitGlueSpec
  P.InternalGlue signedInternalGlue -> evalSignedValue evalInternalGlue signedInternalGlue

evalExplicitGlueSpec :: (Error Eval.EvaluationError :> es, EHexState :> es) => P.ExplicitGlueSpec -> Eff es Q.Glue
evalExplicitGlueSpec P.ExplicitGlueSpec {egLength, egStretch, egShrink} = do
  gDimen <- evalLength egLength
  gStretch <- evalMayFlex egStretch
  gShrink <- evalMayFlex egShrink
  pure $ Q.Glue {gDimen, gStretch, gShrink}

evalMayFlex :: (Error Eval.EvaluationError :> es, EHexState :> es) => Maybe P.PureFlex -> Eff es Q.PureFlex
evalMayFlex = \case
  Nothing -> pure Q.zeroFlex
  Just flex -> evalPureFlex flex

evalPureFlex :: (Error Eval.EvaluationError :> es, EHexState :> es) => P.PureFlex -> Eff es Q.PureFlex
evalPureFlex = \case
  P.FinitePureFlex finiteFlexLength -> Q.FinitePureFlex <$> evalLength finiteFlexLength
  P.InfPureFlex infFlexOfOrder -> Q.InfPureFlex <$> evalInfFlexOfOrder infFlexOfOrder

evalInfFlexOfOrder :: (Error Eval.EvaluationError :> es, EHexState :> es) => P.InfFlexOfOrder -> Eff es Q.InfFlexOfOrder
evalInfFlexOfOrder (P.InfFlexOfOrder signedFactor infFlexOrder) = do
  -- Wrap/unwrap via a 'Sum' newtype, to get the 'Sum' instance of 'Group', so
  -- we get the negative of the rational when needed.
  (Sum factorRational) <- evalSignedValue (fmap Sum . evalFactor) signedFactor
  let factorInfLength = Q.fromBigFils factorRational
  pure $ Q.InfFlexOfOrder factorInfLength infFlexOrder

evalRule ::
  (EHexState :> es, Error Eval.EvaluationError :> es) =>
  P.Rule ->
  Eff es Q.Length ->
  Eff es Q.Length ->
  Eff es Q.Length ->
  Eff es (Box.BoxDims Q.Length)
evalRule (P.Rule dims) defaultW defaultH defaultD = do
  w <- ruleDimen Box.BoxWidth defaultW
  h <- ruleDimen Box.BoxHeight defaultH
  d <- ruleDimen Box.BoxDepth defaultD
  pure $ Box.BoxDims w h d
  where
    ruleDimen d defaultDim = case Seq.filter (\(dim, _len) -> dim == d) dims of
      _ :|> (_, lastDim) ->
        evalLength lastDim
      _ ->
        defaultDim

evalVModeRule ::
  (EHexState :> es, Error Eval.EvaluationError :> es) =>
  P.Rule ->
  Eff es (Box.BoxDims Q.Length)
evalVModeRule rule =
  evalRule rule defaultWidth defaultHeight defaultDepth
  where
    defaultWidth = HSt.getParameterValue (HSt.Param.LengthQuantParam HSt.Param.HSize)
    defaultHeight = pure $ Q.pt 0.4
    defaultDepth = pure Q.zeroLength

evalHModeRule ::
  (EHexState :> es, Error Eval.EvaluationError :> es) =>
  P.Rule ->
  Eff es (Box.BoxDims Q.Length)
evalHModeRule rule =
  evalRule rule defaultWidth defaultHeight defaultDepth
  where
    defaultWidth = pure $ Q.pt 0.4
    defaultHeight = notImplemented "evalHModeRule: defaultHeight"
    defaultDepth = notImplemented "evalHModeRule: defaultDepth"

evalChar ::
  (Error EvaluationError :> es, EHexState :> es) =>
  P.CharCodeRef ->
  Eff es Code.CharCode
evalChar = \case
  P.CharRef c -> pure c
  P.CharTokenRef c -> noteRange c
  P.CharCodeNrRef n -> evalCharCodeInt n

evalCharCodeInt ::
  (Error EvaluationError :> es, EHexState :> es) =>
  P.CharCodeInt ->
  Eff es Code.CharCode
evalCharCodeInt n =
  evalInt n.unCharCodeInt >>= noteRange

noteRange :: (Code.HexCode a, Error EvaluationError :> es) => Q.HexInt -> Eff es a
noteRange x =
  note
    (ValueNotInRange)
    (Code.fromHexInt x)

evalMathLength :: (Error EvaluationError :> es, EHexState :> es) => P.MathLength -> Eff es Q.MathLength
evalMathLength mathLength = evalSignedValue (evalUnsignedMathLength) (mathLength.unMathLength)

evalUnsignedMathLength :: (Error EvaluationError :> es, EHexState :> es) => P.UnsignedMathLength -> Eff es Q.MathLength
evalUnsignedMathLength = \case
  P.NormalMathLengthAsUMathLength normalMathLength -> evalNormalMathLength normalMathLength
  P.CoercedMathLength coercedMathLength -> evalCoercedMathLength coercedMathLength

evalNormalMathLength :: (Error EvaluationError :> es, EHexState :> es) => P.NormalMathLength -> Eff es Q.MathLength
evalNormalMathLength = \case
  P.MathLengthSemiConstant factor mathUnit -> do
    eFactor <- evalFactor factor
    eMathUnit <- evalMathUnit mathUnit
    pure $ Q.scaleMathLengthByRational eFactor eMathUnit

evalMathUnit :: (Error EvaluationError :> es, EHexState :> es) => P.MathUnit -> Eff es Q.MathLength
evalMathUnit = \case
  P.Mu ->
    pure Q.muLength
  P.InternalMathGlueAsUnit internalMathGlue ->
    (.mgDimen) <$> evalInternalMathGlue internalMathGlue

evalCoercedMathLength :: (Error EvaluationError :> es, EHexState :> es) => P.CoercedMathLength -> Eff es Q.MathLength
evalCoercedMathLength = \case
  P.InternalMathGlueAsMathLength internalMathGlue ->
    (.mgDimen) <$> evalInternalMathGlue internalMathGlue

evalMathGlue :: (Error EvaluationError :> es, EHexState :> es) => P.MathGlue -> Eff es Q.MathGlue
evalMathGlue = \case
  P.ExplicitMathGlue mathLength mayMathStretch mayMathShrink -> do
    Q.MathGlue <$> evalMathLength mathLength <*> evalMayMathFlex mayMathStretch <*> evalMayMathFlex mayMathShrink
  P.InternalMathGlue signedInternalMathGlue -> do
    evalSignedValue evalInternalMathGlue signedInternalMathGlue

evalMayMathFlex :: (Error Eval.EvaluationError :> es, EHexState :> es) => Maybe P.PureMathFlex -> Eff es Q.PureMathFlex
evalMayMathFlex = \case
  Nothing -> pure Q.zeroMathFlex
  Just flex -> evalPureMathFlex flex

evalPureMathFlex :: (Error Eval.EvaluationError :> es, EHexState :> es) => P.PureMathFlex -> Eff es Q.PureMathFlex
evalPureMathFlex = \case
  P.FinitePureMathFlex finiteFlexMathLength -> Q.FinitePureMathFlex <$> evalMathLength finiteFlexMathLength
  P.InfPureMathFlex infFlexOfOrder -> Q.InfPureMathFlex <$> evalInfFlexOfOrder infFlexOfOrder

evalTokenListAssignmentTarget :: (Error Eval.EvaluationError :> es, EHexState :> es) => P.TokenListAssignmentTarget -> Eff es HSt.TL.BalancedText
evalTokenListAssignmentTarget = \case
  P.TokenListAssignmentVar var ->
    evalQuantVariableAsTarget var
  P.TokenListAssignmentText balancedText ->
    pure balancedText

evalInternalQuantity ::
  (Error EvaluationError :> es, EHexState :> es) =>
  P.InternalQuantity ->
  Eff es E.InternalQuantity
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

evalInternalLength :: (Error Eval.EvaluationError :> es, EHexState :> es) => P.InternalLength -> Eff es Q.Length
evalInternalLength = \case
  P.InternalLengthVariable lengthVariable ->
    evalQuantVariableAsTarget lengthVariable
  P.InternalSpecialLengthParameter specialLengthParameter ->
    HSt.getSpecialLengthParameter specialLengthParameter
  P.InternalFontDimensionRef _fontDimensionRef ->
    notImplemented "evalInternalLength: InternalFontDimensionRef"
  P.InternalBoxDimensionRef boxDimensionRef ->
    evalBoxDimensionRef boxDimensionRef
  P.LastKern ->
    notImplemented "evalInternalLength: LastKern"

evalBoxDimensionRef :: (Error Eval.EvaluationError :> es, EHexState :> es) => P.BoxDimensionRef -> Eff es Q.Length
evalBoxDimensionRef (P.BoxDimensionRef loc boxDim) = do
  eLoc <- evalExplicitRegisterLocation loc
  HSt.fetchBoxRegisterValue HSt.Reg.Lookup eLoc <&> \case
    Nothing -> Q.zeroLength
    Just (Box.Boxed {boxedDims}) -> case boxDim of
      Box.BoxWidth -> boxedDims.boxWidth
      Box.BoxHeight -> boxedDims.boxHeight
      Box.BoxDepth -> boxedDims.boxDepth

evalInternalGlue :: (Error Eval.EvaluationError :> es, EHexState :> es) => P.InternalGlue -> Eff es Q.Glue
evalInternalGlue = \case
  P.InternalGlueVariable glueVariable ->
    evalQuantVariableAsTarget glueVariable
  P.LastGlue ->
    notImplemented "evalInternalGlue: LastGlue"

evalInternalMathGlue :: (Error Eval.EvaluationError :> es, EHexState :> es) => P.InternalMathGlue -> Eff es Q.MathGlue
evalInternalMathGlue = \case
  P.InternalMathGlueVariable mathGlueVariable ->
    evalQuantVariableAsTarget mathGlueVariable
  P.LastMathGlue ->
    notImplemented "evalInternalGlue: LastMathGlue"

evalFontRef :: EHexState :> es => P.FontRef -> Eff es Font.FontNumber
evalFontRef = \case
  P.FontTokenRef fontNumber -> pure fontNumber
  P.CurrentFontRef -> HSt.currentFontNumber
  P.FamilyMemberFontRef _familyMember -> notImplemented "evalFontRef: FamilyMemberFontRef"

evalFamilyMember :: (Error Eval.EvaluationError :> es, EHexState :> es) => P.FamilyMember -> Eff es HSt.Font.FamilyMember
evalFamilyMember (P.FamilyMember fontRange n) = HSt.Font.FamilyMember fontRange <$> evalInt n
