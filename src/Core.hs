{-# LANGUAGE GeneralisedNewtypeDeriving #-}

module Core
  ( Command (..),
    FSActions (..),
    RealFS,
    runRFS,
    FileInfo (..),
    FSError (..),
  )
where

import Control.Monad.Reader
import Data.IORef (IORef, readIORef, writeIORef)
import Data.Time.Clock (UTCTime)
import System.Directory
import System.FilePath (pathSeparator, takeExtension)

-- | Available commands
data Command
  = Cd FilePath
  | Ls FilePath
  | Mkdir FilePath
  | Touch FilePath
  | Cat FilePath
  | Rmdir FilePath
  | Rm FilePath
  | Writefile FilePath String
  | Find FilePath FilePath
  | InfoFile FilePath
  | Dir
  deriving (Show)

-- | Available errors
data FSError
  = NoSuchFile FilePath
  | NoSuchDir FilePath
  | AlreadyExists FilePath
  | AccessFailure FilePath
  deriving (Show, Eq)

-- | File information
data FileInfo = FileInfo
  { path :: FilePath,
    permissions :: Permissions,
    extension :: String,
    timestamp :: UTCTime,
    size :: Integer
  }

instance Show FileInfo where
  show
    FileInfo
      { path = path',
        permissions = permissions',
        extension = extension',
        timestamp = timestamp',
        size = size'
      } =
      "Path: " ++ path'
        ++ "\nPermissions: "
        ++ show permissions'
        ++ "\nExtension: "
        ++ extension'
        ++ "\nTimestamp: "
        ++ show timestamp'
        ++ "\nSize: "
        ++ show size'

-- | The core "syscall-like" class. Describes file system actions.
class (Monad m) => FSActions m where
  mkdir :: FilePath -> m ()
  infoFile :: FilePath -> m FileInfo
  ls :: FilePath -> m [FilePath]
  isDirExist :: FilePath -> m Bool
  isFileExist :: FilePath -> m Bool
  writeFile :: FilePath -> String -> m ()
  readFile :: FilePath -> m String
  getFullPath :: FilePath -> m FilePath
  makeEmptyFile :: FilePath -> m ()
  cd :: FilePath -> m ()
  rm :: FilePath -> m ()
  rmdir :: FilePath -> m ()
  getPerm :: FilePath -> m Permissions

-- | Type for Real World filesystem
newtype RealFS a = RealFS {runRFS :: ReaderT (IORef FilePath) IO a}
  deriving (Functor, Applicative, Monad, MonadReader (IORef FilePath), MonadIO)

instance FSActions RealFS where
  ls path = liftIO $ listDirectory path
  infoFile path = do
    permissions <- liftIO $ getPermissions path
    let ext = takeExtension path
    time <- liftIO $ getModificationTime path
    sz <- liftIO $ getFileSize path
    return FileInfo {path = path, permissions = permissions, extension = ext, timestamp = time, size = sz}
  isDirExist path = liftIO $ doesDirectoryExist path
  isFileExist path = liftIO $ doesFileExist path
  getFullPath path = do
    curSt <- ask
    curPath <- liftIO $ readIORef curSt
    curPath <- liftIO $ canonicalizePath curPath
    liftIO $ canonicalizePath (curPath ++ [pathSeparator] ++ path)
  writeFile path content = liftIO $ Prelude.writeFile path content
  makeEmptyFile path = liftIO $ Prelude.writeFile path ""
  readFile path = liftIO $ Prelude.readFile path
  mkdir path = liftIO $ createDirectory path
  cd path = do
    curRef <- ask
    liftIO $ writeIORef curRef path
  rm path = liftIO $ removeFile path
  rmdir path = liftIO $ removeDirectoryRecursive path
  getPerm path = liftIO $ getPermissions path
