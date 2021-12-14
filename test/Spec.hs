import           Tests

main :: IO ()
main = do
    testMath
    testNameErr
    testTypeErr
    testList
    testGen
    testOpt
    testOptGen