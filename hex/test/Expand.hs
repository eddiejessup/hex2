{-# OPTIONS_GHC -Wno-missing-methods #-}
{-# OPTIONS_GHC -Wno-orphans #-}

module Expand where

import Hex.Common.Codes (CoreCatCode (..))
import Hex.Common.HexState.Interface qualified as HSt
import Hex.Common.HexState.Interface.Grouped (ScopeFlag (..))
import Hex.Common.HexState.Interface.Parameter qualified as Param
import Hex.Common.Quantity qualified as Q
import Hex.Common.Token.Resolved.Primitive qualified as PT
import Hex.Run.Expand (expandAll)
import Hex.Stage.Expand.Impl
import Hex.Stage.Expand.Interface
import Hexlude
import Lex qualified as T
import Test.Tasty
import Test.Tasty.HUnit

deriving via (PrimTokenSourceT T.TestApp) instance (MonadPrimTokenSource T.TestApp)

expandAllToPTs :: T.TestApp [PT.PrimitiveToken]
expandAllToPTs = do
  foo <- expandAll
  pure $ foo <&> snd

assertSuccessAppEqual :: T.TestApp [PT.PrimitiveToken] -> ByteString -> [PT.PrimitiveToken] -> IO ()
assertSuccessAppEqual app bs expected = do
  res <- T.evalTestApp bs app
  assertEqual
    ""
    expected
    res

assertSuccessExpandEqual :: ByteString -> [PT.PrimitiveToken] -> IO ()
assertSuccessExpandEqual = assertSuccessAppEqual expandAllToPTs

letter :: Char -> PT.PrimitiveToken
letter c = PT.ccp c Letter

other :: Char -> PT.PrimitiveToken
other c = PT.ccp c Other

space :: PT.PrimitiveToken
space = PT.ccp ' ' Space

tests :: TestTree
tests =
  testGroup
    "Expand"
    [ testCase "Usual" usual,
      testCase "Empty input" emptyCase,
      testCase "CSName" csname,
      testCase "String" string,
      testCase "String combined with csname" csnameAndstring
    ]

usual :: Assertion
usual =
  assertSuccessExpandEqual
    "aa"
    [ letter 'a',
      letter 'a',
      space
    ]

emptyCase :: Assertion
emptyCase =
  assertSuccessExpandEqual
    ""
    [ PT.EndParagraphTok
    ]

csname :: Assertion
csname =
  assertSuccessExpandEqual
    "\\csname relax\\endcsname"
    [ PT.RelaxTok
    ]

string :: Assertion
string =
  assertSuccessExpandEqual
    "\\string \\relax"
    [ other '\\',
      other 'r',
      other 'e',
      other 'l',
      other 'a',
      other 'x'
    ]

csnameAndstring :: Assertion
csnameAndstring = do
  T.evalTestApp "\\csname\\string\\lastskip\\endcsname" $ do
    HSt.setParameterValue (Param.IntQuantParam Param.EscapeChar) (Q.HexInt (-1)) Local
    mayPT <- getPrimitiveToken <&> fmap snd
    liftIO $ assertEqual "" (Just PT.LastGlueTok) mayPT
