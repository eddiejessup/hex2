import Expand qualified
import Hexlude
import Lex qualified
import State qualified
import Test.Tasty

main :: IO ()
main = defaultMain tests

tests :: TestTree
tests =
  testGroup
    "Tests"
    [ Lex.tests,
      Expand.tests,
      State.tests
    ]
