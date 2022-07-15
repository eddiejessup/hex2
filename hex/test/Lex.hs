{-# OPTIONS_GHC -Wno-missing-methods #-}

module Lex where

import Hex.Capability.Log.Interface qualified as Log
import Hex.Common.Codes
import Hex.Common.HexEnv.Interface
import Hex.Common.HexInput.Impl
import Hex.Common.HexInput.Interface
import Hex.Common.HexState.Impl
import Hex.Common.HexState.Interface
import Hex.Common.HexState.Interface qualified as HSt
import Hex.Common.HexState.Interface.Grouped (ScopeFlag (Local))
import Hex.Common.Token.Lexed qualified as LT
import Hex.Run.App
import Hex.Run.App qualified as App
import Hex.Run.Lex (lexAll)
import Hexlude
import Test.Tasty
import Test.Tasty.HUnit

newtype TestApp a = TestApp {unTestApp :: StateT AppState (ExceptT AppError IO) a}
  deriving stock (Generic)
  deriving newtype
    ( Functor,
      Applicative,
      Monad,
      MonadError AppError,
      MonadState AppState,
      MonadIO
    )
  deriving (MonadHexInput) via (HexInputT TestApp)
  deriving (MonadHexState) via (HexStateT TestApp)

instance MonadHexEnv TestApp where
  findFilePath = notImplemented "findFilePath"

instance Log.MonadHexLog TestApp where
  log _ = pure ()
  logInternalState = pure ()

runTestApp :: ByteString -> TestApp a -> IO (a, AppState)
runTestApp bs app = do
  st <- App.newAppStateWithChars bs
  let io = runExceptT (runStateT (app.unTestApp) st)
  io >>= \case
    Left appError -> panic $ show appError
    Right a -> pure a

evalTestApp :: ByteString -> TestApp a -> IO a
evalTestApp bs app = fst <$> runTestApp bs app

testLexAll :: ByteString -> IO [LT.LexToken]
testLexAll bs = evalTestApp bs $ do
  HSt.setHexCode CCatCodeType (Chr_ '^') (CoreCatCode Superscript) Local
  lexAll

assertSuccessLexEqual :: ByteString -> [LT.LexToken] -> IO ()
assertSuccessLexEqual inp expected = do
  res <- testLexAll inp
  assertEqual
    ""
    expected
    res

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
    "^^<"
    [ LT.CharCatLexToken (LT.LexCharCat (CharCode (60 + 64)) Other),
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
