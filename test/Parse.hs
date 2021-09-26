module Parse where

import Hex.Codes
import Hex.Lex.Types
import Hex.Syntax.Common
import Hex.Parse.MonadPrimTokenSource.Interface
import Hex.Parse.Parsers.Quantity.Number
import Hex.Quantity qualified as H.Q
import Hex.Symbol.Token.Primitive
import Hexlude
import Test.Tasty
import Test.Tasty.HUnit

data ParseErr = ParseErr ParsingError | EOI
  deriving stock (Show, Eq, Generic)

newtype Parse a = Parse {unParse :: ExceptT ParseErr (StateT [PrimitiveToken] Identity) a}
  deriving stock (Generic)
  deriving newtype (Functor, Applicative, Monad)
  deriving newtype (MonadError ParseErr, MonadState [PrimitiveToken])

instance Alternative Parse where
  empty = Parse $ throwE $ ParseErr ParseFailure

  (<|>) :: Parse a -> Parse a -> Parse a
  a <|> b = Parse $
    ExceptT $ do
      src0 <- get
      aErrOrV <- runExceptT (unParse a)
      case aErrOrV of
        Left _ -> do
          put src0
          bErrOrV <- runExceptT (unParse b)
          case bErrOrV of
            Left _ -> do
              put src0
            _ ->
              pure ()
          pure bErrOrV
        _ ->
          pure aErrOrV

instance MonadPlus Parse

instance MonadPrimTokenSource Parse where
  fetchPT = do
    get >>= \case
      [] -> throwError EOI
      x : xs -> do
        put xs
        pure x

  satisfyThen :: (PrimitiveToken -> Maybe a) -> Parse a
  satisfyThen f = do
    src0 <- get
    t <- fetchPT
    case f t of
      Nothing -> do
        put src0
        empty
      Just v ->
        pure v

  parseError e = throwError $ ParseErr e

tests :: TestTree
tests =
  testGroup
    "Parse"
    [ intTests
    ]

runParser :: [PrimitiveToken] -> Parse a -> (Either ParseErr a, [PrimitiveToken])
runParser pts parser = runIdentity $ runStateT (runExceptT $ unParse parser) pts

assertParseSuccess :: (Show a, Eq a) => [PrimitiveToken] -> Parse a -> a -> IO ()
assertParseSuccess pts parser expected = do
  let (errOrA, remainder) = runParser pts parser
  assertEqual "No remaining input" [] remainder
  case errOrA of
    Left e -> assertFailure $ "Expected success, got failure with error: " <> show e
    Right a -> assertEqual "" expected a

intTests :: TestTree
intTests =
  let nr1ConstAST = HexInt $ Signed [] $ NormalUnsignedInt $ IntConstant $ IntConstantDigits Base10 [1]
   in testGroup
        "Int"
        [ testCase "Decimal digit constant" $
            assertParseSuccess
              [ UnresolvedTok (CharCatLexToken (LexCharCat (Chr_ '1') Other)),
                UnresolvedTok (CharCatLexToken (LexCharCat (Chr_ '2') Other)),
                UnresolvedTok (CharCatLexToken (LexCharCat (Chr_ '3') Other))
              ]
              parseInt
              (HexInt $ Signed [] $ NormalUnsignedInt $ IntConstant $ IntConstantDigits Base10 [1, 2, 3]),
          testCase "Signed decimal digit constant" $
            assertParseSuccess
              [ UnresolvedTok (CharCatLexToken (LexCharCat (Chr_ '-') Other)),
                UnresolvedTok (CharCatLexToken (LexCharCat (Chr_ '+') Other)),
                UnresolvedTok (CharCatLexToken (LexCharCat (Chr_ '+') Other)),
                UnresolvedTok (CharCatLexToken (LexCharCat (Chr_ '1') Other)),
                UnresolvedTok (CharCatLexToken (LexCharCat (Chr_ '2') Other)),
                UnresolvedTok (CharCatLexToken (LexCharCat (Chr_ '3') Other))
              ]
              parseInt
              (HexInt $ Signed [H.Q.Negative, H.Q.Positive, H.Q.Positive] $ NormalUnsignedInt $ IntConstant $ IntConstantDigits Base10 [1, 2, 3]),
          testCase "Octal digit constant" $
            assertParseSuccess
              [ UnresolvedTok (CharCatLexToken (LexCharCat (Chr_ '\'') Other)),
                UnresolvedTok (CharCatLexToken (LexCharCat (Chr_ '1') Other)),
                UnresolvedTok (CharCatLexToken (LexCharCat (Chr_ '2') Other)),
                UnresolvedTok (CharCatLexToken (LexCharCat (Chr_ '3') Other))
              ]
              parseInt
              (HexInt $ Signed [] $ NormalUnsignedInt $ IntConstant $ IntConstantDigits Base8 [1, 2, 3]),
          testCase "Hex digit constant" $
            assertParseSuccess
              [ UnresolvedTok (CharCatLexToken (LexCharCat (Chr_ '"') Other)),
                UnresolvedTok (CharCatLexToken (LexCharCat (Chr_ '1') Other)),
                UnresolvedTok (CharCatLexToken (LexCharCat (Chr_ '2') Other)),
                UnresolvedTok (CharCatLexToken (LexCharCat (Chr_ '3') Other)),
                UnresolvedTok (CharCatLexToken (LexCharCat (Chr_ 'A') Letter))
              ]
              parseInt
              (HexInt $ Signed [] $ NormalUnsignedInt $ IntConstant $ IntConstantDigits Base16 [1, 2, 3, 10]),
          testCase "Char-like: char" $
            assertParseSuccess
              [ UnresolvedTok (CharCatLexToken (LexCharCat (Chr_ '`') Other)),
                UnresolvedTok (CharCatLexToken (LexCharCat (Chr_ 'a') Letter))
              ]
              parseInt
              (HexInt $ Signed [] $ NormalUnsignedInt $ CharLikeCode 97),
          testCase "Char-like: control character" $
            assertParseSuccess
              [ UnresolvedTok (CharCatLexToken (LexCharCat (Chr_ '`') Other)),
                UnresolvedTok (ControlSequenceLexToken (ControlSequence "a"))
              ]
              parseInt
              (HexInt $ Signed [] $ NormalUnsignedInt $ CharLikeCode 97),
          testCase "Internal int-quantity token" $
            assertParseSuccess
              [ LastPenaltyTok
              ]
              parseInt
              (HexInt $ Signed [] $ NormalUnsignedInt $ InternalInt LastPenalty),
          testCase "Internal, variable, parameter" $
            assertParseSuccess
              [ IntParamVarTok PreTolerance
              ]
              parseInt
              (HexInt $ Signed [] $ NormalUnsignedInt $ InternalInt $ InternalIntVariable $ ParamVar PreTolerance),
          testCase "Internal, variable, register, symbolic location" $
            assertParseSuccess
              [ IntRefTok (QuantityType IntQuantity) 1
              ]
              parseInt
              (HexInt $ Signed [] $ NormalUnsignedInt $ InternalInt $ InternalIntVariable $ RegisterVar $ InternalRegisterIndex 1),
          testCase "Internal, variable, register, explicit location" $
            assertParseSuccess
              [ RegisterVariableTok IntQuantity,
                UnresolvedTok (CharCatLexToken (LexCharCat (Chr_ '1') Other))
              ]
              parseInt
              (HexInt $ Signed [] $ NormalUnsignedInt $ InternalInt $ InternalIntVariable $ RegisterVar $ ExplicitRegisterIndex nr1ConstAST),
          testCase "Internal, special" $
            assertParseSuccess
              [ SpecialIntParameterTok SpaceFactorHexInt
              ]
              parseInt
              (HexInt $ Signed [] $ NormalUnsignedInt $ InternalInt $ InternalSpecialIntParameter SpaceFactorHexInt),
          testCase "Internal, code-table reference" $
            assertParseSuccess
              [ CodeTypeTok CategoryCodeType,
                UnresolvedTok (CharCatLexToken (LexCharCat (Chr_ '1') Other))
              ]
              parseInt
              (HexInt $ Signed [] $ NormalUnsignedInt $ InternalInt $ InternalCodeTableRef $ CodeTableRef CategoryCodeType nr1ConstAST),
          testCase "Internal, char token" $
            assertParseSuccess
              [ IntRefTok CharQuantity 1
              ]
              parseInt
              (HexInt $ Signed [] $ NormalUnsignedInt $ InternalInt $ InternalCharToken 1),
          testCase "Internal, math-char token" $
            assertParseSuccess
              [ IntRefTok MathCharQuantity 1
              ]
              parseInt
              (HexInt $ Signed [] $ NormalUnsignedInt $ InternalInt $ InternalMathCharToken 1)
        ]
