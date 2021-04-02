import Test.Tasty
import Test.Tasty.HUnit
import Hexlude
import Categorise qualified
import Lex qualified
import Parse qualified

main :: IO ()
main = defaultMain tests

tests :: TestTree
tests = testGroup "Tests" [Categorise.tests, Lex.tests, Parse.tests]
