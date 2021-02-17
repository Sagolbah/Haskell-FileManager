module Main where

import Control.Exception (catch)
import Control.Monad.Reader (ReaderT (runReaderT))
import Data.IORef (IORef, newIORef, readIORef)
import Handler (executeInReal)
import Options.Applicative
  ( ParserResult (Failure, Success),
    renderFailure,
  )
import Parser (executeParser)
import System.Directory (canonicalizePath, getCurrentDirectory)
import System.Environment (getProgName)
import System.IO (hFlush, stdout)

main :: IO ()
main = do
  initialFileroot <- getCurrentDirectory
  initialFileroot <- canonicalizePath initialFileroot
  rootref <- newIORef initialFileroot
  putStrLn "Starting interactor"
  putStrLn "Use --help for help"
  putStrLn "Use exit to exit"
  shellInteract rootref

shellInteract :: IORef FilePath -> IO ()
shellInteract ref = do
  location <- readIORef ref
  putStr (location ++ " > ")
  hFlush stdout
  str <- getLine
  if str == "exit"
    then return ()
    else do
      let res = executeParser str
      case res of
        (Success cmd) -> executeInReal cmd ref
        (Failure parserHelp) -> putStrLn $ fst $ renderFailure parserHelp ""
        _ -> error "Unknown result"
      shellInteract ref