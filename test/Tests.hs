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
            writeFile "test/resources/Hello.js" (runCodeGen False pg)
            (_, x, err) <- readProcessWithExitCode
                "node"
                ["test/resources/Hello.js"]
                []
            err `shouldBe` ""
            x `shouldBe` "Hello World\n"
        it "io test" $ do
            pg <- readFile "test/resources/Read.scala"
            writeFile "test/resources/Read.js" (runCodeGen False pg)
            let run = readProcessWithExitCode "node" ["test/resources/Read.js"]
            (_, out1, err1) <- run "1\n2"
            err1 `shouldBe` ""
            out1 `shouldBe` "not zero\nbye\n"
            (_, out2, err2) <- run "1\n-1"
            err2 `shouldBe` ""
            out2 `shouldBe` "is zero\nbye\n"

testOpt :: IO ()
testOpt = hspec $ describe "test optimization" $ do
    let file1 = readFile "test/resources/Opt1.scala"
    let cmp f file = do
            result  <- runInterpret False f =<< file
            result' <- runInterpret True f =<< file
            result `shouldSatisfy` isRight
            result' `shouldSatisfy` isRight
            result `shouldBe` result'
    it "test number" $ do
        cmp "adds" file1
        cmp "mix"  file1
    it "test bool" $ do
        cmp "bools" file1
    let file2 = readFile "test/resources/Opt2.scala"
    it "test match" $ do
        cmp "testLitMatch" file2
        cmp "testADTMatch" file2

testOptGen :: IO ()
testOptGen = hspec $ describe "test opt gernerated" $ do
    let
        cmp file input = do
            pg <- readFile $ file ++ ".scala"
            writeFile (file ++ "N.js") (runCodeGen False pg)
            writeFile (file ++ "O.js") (runCodeGen True pg)
            (_, x1, err1) <- readProcessWithExitCode "node"
                                                     [file ++ "N.js"]
                                                     input
            (_, x2, err2) <- readProcessWithExitCode "node"
                                                     [file ++ "O.js"]
                                                     input
            err1 `shouldBe` ""
            err2 `shouldBe` ""
            x2 `shouldBe` x1
    let file1 = "test/resources/OptGen1"
    it "test litfold js" $ do
        cmp file1 []
