module Tests where

import           Data.Either                    ( isLeft )
import           Test.Hspec
import           TestHelper
import           TestUtil
import           Types

testMath :: IO ()
testMath = hspec $ describe "test math" $ do
    let file = readFile "test/resources/Math.scala"
    let run f = runInterpret f =<< file
    it "fib(10)" $ do
        result <- run "testFib"
        result `shouldBe` Right (LitInt (fib 10))
    it "fact(10)" $ do
        result <- run "testFact"
        result `shouldBe` Right (LitInt (fact 10))

testTypeErr :: IO ()
testTypeErr = hspec $ describe "test type errpr" $ do
    let file = readFile "test/resources/TypeErr.scala"
    it "type not check" $ do
        r <- loadProgram <$> file
        r `shouldSatisfy` isLeft

testList :: IO ()
testList = hspec $ describe "test list" $ do
    let run f = do
            fl <- readFile "test/resources/List.scala"
            lb <- readFile "examples/Lib.scala"
            runInterpret f (fl ++ lb)
    it "sum list" $ do
        result <- run "testSum"
        result `shouldBe` Right (LitInt (sum [1 .. 4]))
    it "and list" $ do
        result <- run "testAnd"
        result `shouldBe` Right (LitBool False)
    it "sort test" $ do
        result <- run "testSort"
        let z = (\x -> filter (/= '"') . show $ nameIdx <$> x) <$> result
        let
            out
                = "List.Cons(0, List.Cons(1, List.Cons(2, List.Cons(3, List.Cons(4, List.Cons(6, List.Cons(7, List.Cons(9, List.Nil()))))))))"
        z `shouldBe` Right out
