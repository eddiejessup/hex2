import Expand qualified
import Hexlude
import Lex qualified
import Test.Tasty

main :: IO ()
main = defaultMain tests

tests :: TestTree
tests =
  testGroup
    "Tests"
    [ Lex.tests,
      Expand.tests
    ]
