module Tests where

import           Control.Exception              ( evaluate )
import           Data.Either                    ( isLeft )
import           Test.Hspec
import           TestHelper
import           TestUtil
import           Types

testMath :: IO ()
testMath = hspec $ do
    describe "test math" $ do
        let file = readFile "test/resources/Math.scala"
        let run f = runInterpret f =<< file
        it "fib(10)" $ do
            result <- run "testFib"
            result `shouldBe` Right (LitInt (fib 10))
        it "fact(10)" $ do
            result <- run "testFact"
            result `shouldBe` Right (LitInt (fact 10))

testTypeErr :: IO ()
testTypeErr = hspec $ do
    describe "test typeerr" $ do
        let file = readFile "test/resources/TypeErr.scala"
        it "type not check" $ do
            r <- loadProgram <$> file
            r `shouldSatisfy` isLeft
