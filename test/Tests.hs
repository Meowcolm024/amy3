module Tests where

import           Data.Either                    ( isRight )
import           System.Process
import           Test.Hspec
import           TestHelper
import           TestUtil
import           Types

testMath :: IO ()
testMath = hspec $ describe "test math" $ do
    let file = readFile "test/resources/Math.scala"
    let run f = runInterpret False f =<< file
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
    let file3 = readFile "test/resources/TypeErr3.scala"
    it "type not check" $ do
        r <- loadProgram <$> file
        r `shouldNotSatisfy` isRight
        r2 <- loadProgram <$> file2
        r2 `shouldNotSatisfy` isRight
        r3 <- loadProgram <$> file3
        r3 `shouldNotSatisfy` isRight

testList :: IO ()
testList = hspec $ describe "test list" $ do
    let run f = do
            fl <- readFile "test/resources/List.scala"
            lb <- readFile "examples/Lib.scala"
            runInterpret False f (fl ++ lb)
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
            (_, x, err) <- readProcessWithExitCode
                "node"
                ["test/resources/Hello.js"]
                []
            err `shouldBe` ""
            x `shouldBe` "Hello World\n"
        it "io test" $ do
            pg <- readFile "test/resources/Read.scala"
            writeFile "test/resources/Read.js" (runCodeGen pg)
            let run = readProcessWithExitCode "node" ["test/resources/Read.js"]
            (_, out1, err1) <- run "1\n2"
            err1 `shouldBe` ""
            out1 `shouldBe` "not zero\nbye\n"
            (_, out2, err2) <- run "1\n-1"
            err2 `shouldBe` ""
            out2 `shouldBe` "is zero\nbye\n"

testOpt :: IO ()
testOpt = hspec $ describe "test optimization" $ do
    let file = readFile "test/resources/Opt1.scala"
    let cmp f = do
            result <- runInterpret False f =<< file
            result' <- runInterpret True f =<< file
            result `shouldBe` result'
    it "test number" $ do
        cmp "adds"
        cmp "mix"
    it "test bool" $ do
        cmp "bools"
