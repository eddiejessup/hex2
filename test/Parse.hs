module Parse where

import Hex.Codes
import Hex.Lex.Types
import Hex.Parse.AST
import Hex.Parse.MonadPrimTokenSource.Interface
import Hex.Parse.Parsers.Combinators
import Hex.Parse.Parsers.Quantity
import Hex.Quantity qualified as H.Q
import Hex.Symbol.Tokens
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
    "Parsing"
    [ intTests,
      internalIntTests
    ]

runParser :: [PrimitiveToken] -> Parse a -> (Either ParseErr a, [PrimitiveToken])
runParser pts parser = runIdentity $ runStateT (runExceptT $ unParse parser) pts

assertParseSuccess :: (Show a, Eq a) => [PrimitiveToken] -> Parse a -> a -> IO ()
assertParseSuccess pts parser a = do
  let (errOrA, remainder) = runParser pts parser
  assertEqual "No remaining input" [] remainder
  assertEqual "" (Right a) errOrA

intTests :: TestTree
intTests =
  testGroup
    "Int"
    [ testCase "Decimal digit constant" $
        assertParseSuccess
          [ UnresolvedTok (CharCatLexToken (LexCharCat (CharCode_ '1') Other)),
            UnresolvedTok (CharCatLexToken (LexCharCat (CharCode_ '2') Other)),
            UnresolvedTok (CharCatLexToken (LexCharCat (CharCode_ '3') Other))
          ]
          parseInt
          (HexInt $ Signed [] $ NormalUnsignedInt $ IntConstant $ IntConstantDigits Base10 [1, 2, 3]),
      testCase "Signed decimal digit constant" $
        assertParseSuccess
          [ UnresolvedTok (CharCatLexToken (LexCharCat (CharCode_ '-') Other)),
            UnresolvedTok (CharCatLexToken (LexCharCat (CharCode_ '+') Other)),
            UnresolvedTok (CharCatLexToken (LexCharCat (CharCode_ '+') Other)),
            UnresolvedTok (CharCatLexToken (LexCharCat (CharCode_ '1') Other)),
            UnresolvedTok (CharCatLexToken (LexCharCat (CharCode_ '2') Other)),
            UnresolvedTok (CharCatLexToken (LexCharCat (CharCode_ '3') Other))
          ]
          parseInt
          (HexInt $ Signed [H.Q.Negative, H.Q.Positive, H.Q.Positive] $ NormalUnsignedInt $ IntConstant $ IntConstantDigits Base10 [1, 2, 3]),
      testCase "Octal digit constant" $
        assertParseSuccess
          [ UnresolvedTok (CharCatLexToken (LexCharCat (CharCode_ '\'') Other)),
            UnresolvedTok (CharCatLexToken (LexCharCat (CharCode_ '1') Other)),
            UnresolvedTok (CharCatLexToken (LexCharCat (CharCode_ '2') Other)),
            UnresolvedTok (CharCatLexToken (LexCharCat (CharCode_ '3') Other))
          ]
          parseInt
          (HexInt $ Signed [] $ NormalUnsignedInt $ IntConstant $ IntConstantDigits Base8 [1, 2, 3]),
      testCase "Hex digit constant" $
        assertParseSuccess
          [ UnresolvedTok (CharCatLexToken (LexCharCat (CharCode_ '"') Other)),
            UnresolvedTok (CharCatLexToken (LexCharCat (CharCode_ '1') Other)),
            UnresolvedTok (CharCatLexToken (LexCharCat (CharCode_ '2') Other)),
            UnresolvedTok (CharCatLexToken (LexCharCat (CharCode_ '3') Other)),
            UnresolvedTok (CharCatLexToken (LexCharCat (CharCode_ 'A') Letter))
          ]
          parseInt
          (HexInt $ Signed [] $ NormalUnsignedInt $ IntConstant $ IntConstantDigits Base16 [1, 2, 3, 10]),
      testCase "Char-like: char" $
        assertParseSuccess
          [ UnresolvedTok (CharCatLexToken (LexCharCat (CharCode_ '`') Other)),
            UnresolvedTok (CharCatLexToken (LexCharCat (CharCode_ 'a') Letter))
          ]
          parseInt
          (HexInt $ Signed [] $ NormalUnsignedInt $ CharLikeCode 97),
      testCase "Char-like: control character" $
        assertParseSuccess
          [ UnresolvedTok (CharCatLexToken (LexCharCat (CharCode_ '`') Other)),
            UnresolvedTok (ControlSequenceLexToken (ControlSequence "a"))
          ]
          parseInt
          (HexInt $ Signed [] $ NormalUnsignedInt $ CharLikeCode 97)
    ]

internalIntTests :: TestTree
internalIntTests =
  testGroup
    "Internal int"
    [ testCase "Quantity token" $
        assertParseSuccess
          [ LastPenaltyTok
          ]
          (parseHeaded headToParseInternalInt)
          LastPenalty,
      testCase "Variable, parameter" $
        assertParseSuccess
          [ IntParamVarTok HBadness
          ]
          (parseHeaded headToParseInternalInt)
          (InternalIntVariable (ParamVar HBadness)),
      testCase "Variable, int reference" $
        assertParseSuccess
          [ IntRefTok (QuantityType IntQuantity) 2
          ]
          (parseHeaded headToParseInternalInt)
          (InternalIntVariable $ RegisterVar $ InternalRegisterLocation 2)
    ]
