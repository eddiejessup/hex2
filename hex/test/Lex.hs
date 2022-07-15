{-# OPTIONS_GHC -Wno-missing-methods #-}

module Lex where

import Hex.Capability.Log.Interface qualified as Log
import Hex.Common.Codes
import Hex.Common.HexEnv.Interface
import Hex.Common.HexInput.Impl
import Hex.Common.HexInput.Impl.CharSourceStack
import Hex.Common.HexInput.Impl.CharSourceStack qualified as HIn
import Hex.Common.HexInput.Interface
import Hex.Common.HexState.Impl.Defaults.Parameter (newEndLineChar)
import Hex.Common.HexState.Interface
import Hex.Common.HexState.Interface.Parameter qualified as Param
import Hex.Common.HexState.Interface.Variable qualified as Var
import Hex.Common.Token.Lexed qualified as LT
import Hex.Run.Lex (lexAll)
import Hexlude
import Test.Tasty
import Test.Tasty.HUnit

newtype TestApp a = TestApp {unTestApp :: StateT CharSourceStack (ExceptT (Identity LexError) IO) a}
  deriving stock (Generic)
  deriving newtype
    ( Functor,
      Applicative,
      Monad,
      MonadError (Identity LexError),
      MonadState CharSourceStack,
      MonadIO
    )
  deriving (MonadHexInput) via (HexInputT TestApp)

instance MonadHexEnv TestApp where
  findFilePath = notImplemented "findFilePath"

codeToCat :: CharCode -> CatCode
codeToCat = \case
  Chr_ '\\' -> Escape
  Chr_ ' ' -> CoreCatCode Space
  Chr_ '%' -> Comment
  Chr_ '\r' -> EndOfLine
  Chr_ '^' -> CoreCatCode Superscript
  Chr_ 'a' -> CoreCatCode Letter
  Chr_ 'b' -> CoreCatCode Letter
  _ -> CoreCatCode Other

instance Log.MonadHexLog TestApp where
  log _ = pure ()
  logInternalState = pure ()

instance MonadHexState TestApp where
  getHexCode CCatCodeType code = pure $ codeToCat code
  getHexCode _ _ = notImplemented "getHexCode"

  getParameterValue :: Param.QuantParam q -> TestApp (Var.QuantVariableTarget q)
  getParameterValue (Param.IntQuantParam Param.EndLineChar) = pure newEndLineChar
  getParameterValue _ = notImplemented "getParameterValue"

runTestApp :: ByteString -> TestApp a -> IO a
runTestApp bs app = do
  let stack = HIn.newCharSourceStack (Just (Chr_ '\r')) bs
      io = runExceptT (evalStateT (app.unTestApp) stack)
  io >>= \case
    Left (Identity err) -> panic $ show err
    Right a -> pure a

testLexAll :: ByteString -> IO [LT.LexToken]
testLexAll bs = runTestApp bs lexAll

assertSuccessLexEqual :: ByteString -> [LT.LexToken] -> IO ()
assertSuccessLexEqual inp expected = do
  res <- testLexAll inp
  assertEqual
    ""
    res
    expected

letter :: Char -> LT.LexToken
letter c = LT.ccLex c Letter

space :: LT.LexToken
space = LT.ccLex ' ' Space

tests :: TestTree
tests =
  testGroup
    "Lex"
    [ testsTrioing,
      testsSpacesNewlinesWord,
      testControlSequences
    ]

testsTrioing :: TestTree
testsTrioing =
  testGroup
    "Trioing"
    [ testCase "Usual" usual,
      testCase "One caret" oneCaret,
      testCase "Two carets" twoCarets,
      testCase "Triod up" triodUp,
      testCase "Triod down" triodDown
    ]

usual :: Assertion
usual = do
  assertSuccessLexEqual
    "aa"
    [ letter 'a',
      letter 'a',
      space
    ]

oneCaret :: Assertion
oneCaret = do
  assertSuccessLexEqual
    "^"
    [ LT.ccLex '^' Superscript,
      space
    ]
  assertSuccessLexEqual
    "^a"
    [ LT.ccLex '^' Superscript,
      letter 'a',
      space
    ]

twoCarets :: Assertion
twoCarets = do
  assertSuccessLexEqual
    "^^"
    [ LT.ccLex '^' Superscript,
      LT.ccLex '^' Superscript,
      space
    ]

triodUp :: Assertion
triodUp = do
  assertSuccessLexEqual
    "^^?"
    [ LT.CharCatLexToken (LT.LexCharCat (CharCode (63 + 64)) Other),
      space
    ]

triodDown :: Assertion
triodDown = do
  assertSuccessLexEqual
    "^^A"
    [ LT.CharCatLexToken (LT.LexCharCat (CharCode (65 - 64)) Other),
      space
    ]
  assertSuccessLexEqual
    "^^^"
    [ LT.CharCatLexToken (LT.LexCharCat (CharCode (94 - 64)) Other),
      space
    ]

testsSpacesNewlinesWord :: TestTree
testsSpacesNewlinesWord =
  testGroup
    "Spaces, newlines, words"
    [ testCase "Chars" testChars,
      testCase "Words" testWords,
      testCase "Multiple spaces" testMultipleSpaces,
      testCase "Comment" testComment,
      testCase "Spaces at beginning of line" testSpacesLineBegin,
      testCase "New-line while skipping blanks" testNewLineSkippingBlanks
    ]

testChars :: Assertion
testChars =
  assertSuccessLexEqual
    "aa"
    [ letter 'a',
      letter 'a',
      space
    ]

testWords :: Assertion
testWords =
  assertSuccessLexEqual
    "aa aa aa"
    [ letter 'a',
      letter 'a',
      space,
      letter 'a',
      letter 'a',
      space,
      letter 'a',
      letter 'a',
      space
    ]

testMultipleSpaces :: Assertion
testMultipleSpaces =
  assertSuccessLexEqual
    "aa     aa"
    [ letter 'a',
      letter 'a',
      space,
      letter 'a',
      letter 'a',
      space
    ]

testComment :: Assertion
testComment =
  assertSuccessLexEqual
    "aa%c1c1c1\nbb"
    [ letter 'a',
      letter 'a',
      letter 'b',
      letter 'b',
      space
    ]

testSpacesLineBegin :: Assertion
testSpacesLineBegin =
  assertSuccessLexEqual
    "   aa"
    [ letter 'a',
      letter 'a',
      space
    ]

testNewLineSkippingBlanks :: Assertion
testNewLineSkippingBlanks =
  assertSuccessLexEqual
    "a  \na"
    [ letter 'a',
      space,
      letter 'a',
      space
    ]

testControlSequences :: TestTree
testControlSequences =
  testGroup
    "Control word"
    [ testCase "Alone" $
        assertSuccessLexEqual
          "\\abab"
          [ LT.csLex "abab"
          ],
      testCase "Control word with following digit" $
        assertSuccessLexEqual
          "\\abab1"
          [ LT.csLex "abab",
            LT.ccLex '1' Other,
            space
          ],
      testCase "Control word with following space" $
        assertSuccessLexEqual
          "\\abab "
          [ LT.csLex "abab"
          ],
      testCase "Control letter-character alone" $
        assertSuccessLexEqual
          "\\a"
          [ LT.csLex "a"
          ],
      testCase "Control letter-character with following digit" $
        assertSuccessLexEqual
          "\\a1"
          [ LT.csLex "a",
            LT.ccLex '1' Other,
            space
          ],
      testCase "Control letter-character with following space" $
        assertSuccessLexEqual
          "\\a "
          [ LT.csLex "a"
          ],
      testCase "Control other-character alone" $
        assertSuccessLexEqual
          "\\1"
          [ LT.csLex "1",
            space
          ],
      testCase "Control other-character with following digit" $
        assertSuccessLexEqual
          "\\11"
          [ LT.csLex "1",
            LT.ccLex '1' Other,
            space
          ],
      testCase "Control other-character with following space" $
        assertSuccessLexEqual
          "\\1 "
          [ LT.csLex "1",
            space
          ]
    ]
