import Test.Tasty (defaultMain, testGroup)
import FMTesting (testFM)

main :: IO ()
main = do
  fmtest <- FMTesting.testFM
  defaultMain $ testGroup "Correctness" [fmtest]