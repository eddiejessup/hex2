import Categorise qualified
import Hexlude
import Lex qualified
import Parse qualified
import Test.Tasty
import Test.Tasty.HUnit

main :: IO ()
main = defaultMain tests

tests :: TestTree
tests = testGroup "Tests" [Categorise.tests, Lex.tests, Parse.tests]
