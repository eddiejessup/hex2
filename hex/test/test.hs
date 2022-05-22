import Categorise qualified
import Hexlude
import Lex qualified
import Test.Tasty

main :: IO ()
main = defaultMain tests

tests :: TestTree
tests = testGroup "Tests" [Categorise.tests, Lex.tests]
