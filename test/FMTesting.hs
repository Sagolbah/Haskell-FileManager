module FMTesting
  ( testFM,
  )
where

import Control.Monad.Except (runExceptT)
import Control.Monad.State (evalState, runState)
import Core
import Data.Time.Clock (UTCTime, utctDayTime)
import Handler (executeCat, executeFind, executeLS, executeMkdir, executeCd)
import MockFS
    ( MockFS(runMockFS), MockFile(MockFile), MockDir(MockDir), mockPermissions  )
import System.Directory (Permissions, executable, readable, searchable, writable)
import Test.Tasty (TestTree)
import Test.Tasty.Hspec (describe, it, shouldBe, testSpec)

defaultTime :: UTCTime
defaultTime = read "2014-12-19 13:20:51.601234 UTC" :: UTCTime

defaultInfo :: String -> FileInfo
defaultInfo st = FileInfo st mockPermissions st defaultTime 1

-- I decided not to implement WriteFile test.
-- Realized that changing list value is painful, and it's too late to refactor for Map :(
testFM :: IO TestTree
testFM =
  testSpec "File Manager Unit Set" $ do
    let f1 = MockFile "abc" (defaultInfo "abc") "hello"
    let f2 = MockFile "abc2" (defaultInfo "abc2") "hello2"
    let fileNested = MockFile "abc3" (defaultInfo "abc3") "hello3"
    let dirNested = MockDir "nested" [Right fileNested]
    let f3 = MockFile "728Patch" (defaultInfo "728Patch") "PudgeRemovedFromGame"
    let dir = MockDir "dir" [Right f1, Right f2, Left dirNested]
    let dir2 = MockDir "dir2" []
    let root = MockDir "root" [Left dir, Left dir2, Right f3]
    describe "ls" $ do
      it "shows dir content" $ do
        let res = evalState (runExceptT (runMockFS (executeLS ""))) root
        res `shouldBe` Right (Right ["dir", "dir2", "728Patch"])
        let res' = evalState (runExceptT (runMockFS (executeLS "dir"))) root
        res' `shouldBe` Right (Right ["abc", "abc2", "nested"])
    describe "cat" $ do
      it "shows content" $ do
        let res = evalState (runExceptT (runMockFS (executeCat "728Patch"))) root
        res `shouldBe` Right (Right "PudgeRemovedFromGame")
        let res = evalState (runExceptT (runMockFS (executeCat "dir/abc"))) root
        res `shouldBe` Right (Right "hello")
    describe "mkdir" $ do
      it "creates directories" $ do
        let st = runState (runExceptT (runMockFS (executeMkdir "newdir"))) root
        let res = evalState (runExceptT (runMockFS (executeLS ""))) (snd st)
        res `shouldBe` Right (Right ["newdir", "dir", "dir2", "728Patch"])
    describe "find" $ do
      it "searches in directories" $ do
        let res = evalState (runExceptT (runMockFS (executeFind "dir" "abc3"))) root
        let res' = evalState (runExceptT (runMockFS (executeFind "dir" "aqqq"))) root
        res `shouldBe` Right (Right (Just "dir/nested"))
        res' `shouldBe` Right (Right Nothing)
    describe "cd" $ do
      it "changes directories" $ do
        let st = runState (runExceptT (runMockFS (executeCd "dir/nested"))) root
        let res = evalState (runExceptT (runMockFS (executeLS ""))) (snd st)
        res `shouldBe` Right (Right ["abc3"])
