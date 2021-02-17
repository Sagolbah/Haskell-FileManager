module Handler
  ( executeInReal,
    executeLS,
    executeCat,
    executeMkdir,
    executeFind,
    executeCd
  )
where

import Control.Monad.Reader (ReaderT (runReaderT), filterM, liftIO)
import Core
import Data.IORef (IORef)
import Data.List (intercalate)
import System.Directory (Permissions (readable))
import System.FilePath (pathSeparator)

-- | Shows all files in directories (excluding . and ..)
executeLS :: (FSActions m) => FilePath -> m (Either FSError [FilePath])
executeLS path = do
  fullPath <- getFullPath path
  isExist <- isDirExist fullPath
  if isExist
    then do
      res <- ls fullPath
      return (Right res)
    else return (Left (NoSuchDir fullPath))

-- | Returns file information - path, permissions, extension, timestamp and size.
executeFileInfo :: (FSActions m) => FilePath -> m (Either FSError FileInfo)
executeFileInfo path = do
  fullPath <- getFullPath path
  isExist <- isFileExist fullPath
  if isExist
    then do
      res <- infoFile fullPath
      return (Right res)
    else return (Left (NoSuchFile fullPath))

-- | Inserts text into existing file
executeWriteFile :: (FSActions m) => FilePath -> String -> m (Either FSError ())
executeWriteFile path content = do
  fullPath <- getFullPath path
  isExist <- isFileExist fullPath
  if isExist
    then do
      perm <- getPerm fullPath
      if readable perm
        then do
          res <- Core.writeFile fullPath content
          return (Right ())
        else return (Left (AccessFailure fullPath))
    else return (Left (NoSuchFile fullPath))

-- | Returns contents of file and error if file not found / not readable
executeCat :: (FSActions m) => FilePath -> m (Either FSError String)
executeCat path = do
  fullPath <- getFullPath path
  isExist <- isFileExist fullPath
  if isExist
    then do
      perm <- getPerm fullPath
      if readable perm
        then do
          res <- Core.readFile fullPath
          return (Right res)
        else return (Left (AccessFailure fullPath))
    else return (Left (NoSuchFile fullPath))

-- | Creates empty file in file system
executeTouch :: (FSActions m) => FilePath -> m (Either FSError ())
executeTouch path = do
  fullPath <- getFullPath path
  isExist <- isFileExist fullPath
  if not isExist
    then do
      makeEmptyFile fullPath
      return (Right ())
    else return (Left (AlreadyExists fullPath))

-- | Creates directory in file system
executeMkdir :: (FSActions m) => FilePath -> m (Either FSError ())
executeMkdir path = do
  fullPath <- getFullPath path
  isExist <- isDirExist fullPath
  if isExist
    then return (Left (AlreadyExists fullPath))
    else do
      res <- mkdir fullPath
      return (Right res)

-- | Changes directory
executeCd :: (FSActions m) => FilePath -> m (Either FSError String)
executeCd path = do
  fullPath <- getFullPath path
  isExist <- isDirExist fullPath
  if isExist
    then do
      cd fullPath
      return (Right ("Changed dir to " ++ path))
    else return (Left (NoSuchDir fullPath))

-- | Removes file in filesystem
executeRm :: (FSActions m) => FilePath -> m (Either FSError ())
executeRm path = do
  fullPath <- getFullPath path
  isExist <- isFileExist fullPath
  if isExist
    then do
      rm fullPath
      return (Right ())
    else return (Left (NoSuchFile fullPath))

-- | Removes directory in filesystem
executeRmdir :: (FSActions m) => FilePath -> m (Either FSError ())
executeRmdir path = do
  fullPath <- getFullPath path
  isExist <- isDirExist fullPath
  if isExist
    then do
      rmdir fullPath
      return (Right ())
    else return (Left (NoSuchDir fullPath))

-- | Searches for given file in directory recursively. Returns path to it or Nothing.
executeFind :: (FSActions m) => FilePath -> FilePath -> m (Either FSError (Maybe FilePath))
executeFind path name = do
  fullPath <- getFullPath path
  isExist <- isDirExist fullPath
  if isExist
    then do
      permissions <- getPerm fullPath
      if not (readable permissions)
        then return (Left (AccessFailure fullPath))
        else do
          res <- findHelper fullPath name
          return (Right res)
    else return (Left (NoSuchDir fullPath))

findHelper :: (FSActions m) => FilePath -> FilePath -> m (Maybe FilePath)
findHelper fullpath name = do
  fileList <- ls fullpath
  isFileList <- mapM isDirExist fileList
  existence <- isFileExist (composePaths fullpath name)
  if existence
    then return (Just fullpath)
    else do
      dirs <- filterM (isDirExist . composePaths fullpath) fileList
      results <- mapM (\x -> findHelper (composePaths fullpath x) name) dirs
      return $ hasMaybe results

-- | Utility function for find
composePaths :: FilePath -> FilePath -> FilePath
composePaths x y = x ++ [pathSeparator] ++ y

-- | Utility function for find
hasMaybe :: [Maybe FilePath] -> Maybe FilePath
hasMaybe [] = Nothing
hasMaybe (Nothing : xs) = hasMaybe xs
hasMaybe (x : xs) = x

-- | Receives command and executes it in real world
executeInReal :: Command -> IORef FilePath -> IO ()
executeInReal Dir ref = executeInReal (Ls "") ref
executeInReal (Ls path) ref = do
  res <- runReaderT (runRFS (executeLS path)) ref
  showResult res (intercalate "\n")
executeInReal (InfoFile path) ref = do
  res <- runReaderT (runRFS (executeFileInfo path)) ref
  showResult res show
executeInReal (Writefile path content) ref = do
  res <- runReaderT (runRFS (executeWriteFile path content)) ref
  showResult res (const "Success")
executeInReal (Cat path) ref = do
  res <- runReaderT (runRFS (executeCat path)) ref
  showResult res id
executeInReal (Touch path) ref = do
  res <- runReaderT (runRFS (executeTouch path)) ref
  showResult res (const "Success")
executeInReal (Mkdir path) ref = do
  res <- runReaderT (runRFS (executeMkdir path)) ref
  showResult res (const "Success")
executeInReal (Cd path) ref = do
  res <- runReaderT (runRFS (executeCd path)) ref
  showResult res id
executeInReal (Rm path) ref = do
  res <- runReaderT (runRFS (executeRm path)) ref
  showResult res (const "Success")
executeInReal (Rmdir path) ref = do
  res <- runReaderT (runRFS (executeRmdir path)) ref
  showResult res (const "Success")
executeInReal (Find dir name) ref = do
  res <- runReaderT (runRFS (executeFind dir name)) ref
  case res of
    Left err -> print err
    Right x -> case x of
      Just location -> putStrLn location
      _ -> putStrLn "Not found"

-- | Shows received result during real-world command execution
showResult :: Either FSError a -> (a -> String) -> IO ()
showResult (Left err) _ = print err
showResult (Right x) fn = putStrLn (fn x)
