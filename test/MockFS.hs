{-# LANGUAGE GeneralisedNewtypeDeriving #-}

module MockFS where

import Control.Monad.Except (ExceptT, lift)
import Control.Monad.State (MonadState, State, get, lift, put)
import Core
import Data.List.Split (splitOn)
import Data.Maybe (fromJust, isJust)
import System.Directory (Permissions, emptyPermissions, setOwnerReadable, setOwnerWritable)

data MockDir = MockDir {name :: String, content :: [MockFSElement]}

data MockFile = MockFile {filename :: String, info :: FileInfo, text :: String}

type MockFSElement = Either MockDir MockFile

-- | Mock filesystem
newtype MockFS a = MockFS {runMockFS :: ExceptT FSError (State MockDir) a}
  deriving (Functor, Applicative, Monad, (MonadState MockDir))

-- | Returns permissions for reading and writing
mockPermissions :: Permissions
mockPermissions = do
  let perm = emptyPermissions
  let perm' = setOwnerReadable True perm
  let perm'' = setOwnerWritable True perm'
  perm''

instance FSActions MockFS where
  getFullPath path = return path
  isDirExist path = do
    fsSt <- get
    return $ isJust $ searchDirWithPath fsSt path
  isFileExist path = do
    fsSt <- get
    return $ isJust $ searchFileWithPath fsSt path
  ls path = do
    fsSt <- get
    let newdirCont = content $ fromJust $ searchDirWithPath fsSt path
    let ans = map getName newdirCont
    return ans
  getPerm path = return mockPermissions
  readFile path = do
    fsSt <- get
    let curFile = fromJust $ searchFileWithPath fsSt path
    return $ text curFile
  mkdir path = do
    -- :NOTE: Can do it only in current directory!
    fsSt <- get
    let ct = content fsSt
    let newDir = Left $ MockDir path []
    let newFS = fsSt {content = newDir : ct}
    put newFS
  cd path = do
    fsSt <- get
    let newDir = fromJust $ searchDirWithPath fsSt path
    put newDir
  writeFile = error "Not implemented"
  infoFile = error "Not implemented"
  rm = error "Not implemented"
  makeEmptyFile = error "Not implemented"
  rmdir = error "Not implemented"

getName :: MockFSElement -> String
getName (Left x) = name x
getName (Right x) = filename x

searchDir :: [MockFSElement] -> FilePath -> Maybe MockDir
searchDir [] _ = Nothing
searchDir (x : xs) target
  | getName x /= target = searchDir xs target
  | otherwise = case x of
    Left y -> Just y
    _ -> searchDir xs target

searchDirWithPath :: MockDir -> FilePath -> Maybe MockDir
searchDirWithPath dir "" = Just dir
searchDirWithPath dir path = helper dir (splitOn "/" path)
  where
    helper dir [] = Just dir
    helper dir (x : xs) = helper2 (searchDir (content dir) x) xs
    helper2 Nothing _ = Nothing
    helper2 (Just x) xs = helper x xs

searchFile :: [MockFSElement] -> FilePath -> Maybe MockFile
searchFile [] _ = Nothing
searchFile (x : xs) target
  | getName x /= target = searchFile xs target
  | otherwise = case x of
    Right y -> Just y
    _ -> searchFile xs target

searchFileWithPath :: MockDir -> FilePath -> Maybe MockFile
searchFileWithPath dir "" = Nothing
searchFileWithPath dir path = helper dir (init splitted)
  where
    splitted = splitOn "/" path
    final = last splitted
    helper dir [] = searchFile (content dir) final
    helper dir (x : xs) = helper2 (searchDir (content dir) x) xs
    helper2 Nothing _ = Nothing
    helper2 (Just x) xs = helper x xs