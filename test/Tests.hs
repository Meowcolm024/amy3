module Tests where

import           Data.Either                    ( isLeft )
import           System.Process
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
    let file  = readFile "test/resources/TypeErr.scala"
    let file2 = readFile "test/resources/TypeErr2.scala"
    it "type not check" $ do
        r <- loadProgram <$> file
        r `shouldSatisfy` isLeft
        r2 <- loadProgram <$> file2
        r2 `shouldSatisfy` isLeft

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
    let
        out =
            "List.Cons(0, List.Cons(1, List.Cons(2, List.Cons(3,"
                ++ " List.Cons(4, List.Cons(6, List.Cons(7, List.Cons(9, List.Nil()))))))))"
    it "sort test" $ do
        result <- run "testSort"
        printExpr <$> result `shouldBe` Right out
    it "flatten test" $ do
        result <- run "testFlat"
        printExpr <$> result `shouldBe` Right out

testGen :: IO ()
testGen = do
    hspec $ describe "test gen" $ do
        it "hello test" $ do
            pg <- readFile "test/resources/Hello.scala"
            writeFile "test/resources/Hello.js" (runCodeGen pg)
            x <- readProcess "node" ["test/resources/Hello.js"] []
            x `shouldBe` "Hello World\n"
        it "io test" $ do
            pg <- readFile "test/resources/Read.scala"
            writeFile "test/resources/Read.js" (runCodeGen pg)
            let run = readProcessWithExitCode "node" ["test/resources/Read.js"]
            (_, out1, _) <- run "1\n2"
            out1 `shouldBe` "not zero\nbye\n"
            (_, out2, _) <- run "1\n-1"
            out2 `shouldBe` "is zero\nbye\n"
