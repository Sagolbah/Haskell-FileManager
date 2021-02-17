module Parser
  ( executeParser,
  )
where

import Core (Command (..))
import Options.Applicative

executeParser :: String -> ParserResult Command
executeParser input = execParserPure defaultPrefs helperData (words input)
  where
    helperData = info (helper <*> commandParser) (fullDesc <> progDesc "Shell command for File Manager")

commandParser :: Parser Command
commandParser =
  subparser $
    command "ls" (info (helper <*> lsBody) (fullDesc <> progDesc "Show directory content"))
      <> command "touch" (info (helper <*> touchBody) (fullDesc <> progDesc "Create empty file"))
      <> command "infofile" (info (helper <*> infofileBody) (fullDesc <> progDesc "Show file information"))
      <> command "dir" (info (helper <*> dirBody) (fullDesc <> progDesc "Show content of current directory"))
      <> command "writefile" (info (helper <*> writefileBody) (fullDesc <> progDesc "Write content to existing file"))
      <> command "cat" (info (helper <*> catBody) (fullDesc <> progDesc "Get and print content of file"))
      <> command "mkdir" (info (helper <*> mkdirBody) (fullDesc <> progDesc "Create empty directory"))
      <> command "cd" (info (helper <*> cdBody) (fullDesc <> progDesc "Change directory"))
      <> command "rm" (info (helper <*> rmBody) (fullDesc <> progDesc "Remove file"))
      <> command "rmdir" (info (helper <*> rmdirBody) (fullDesc <> progDesc "Remove directory recursively"))
      <> command "find" (info (helper <*> findBody) (fullDesc <> progDesc "Search file in directory"))
  where
    dirArgument = argument str (metavar "DIR" <> help "path to directory")
    fileArgument = argument str (metavar "FILE" <> help "name of file")
    textArgument = argument str (metavar "TEXT" <> help "text")
    lsBody = Ls <$> dirArgument
    touchBody = Touch <$> fileArgument
    infofileBody = InfoFile <$> fileArgument
    catBody = Cat <$> fileArgument
    mkdirBody = Mkdir <$> fileArgument
    writefileBody = Writefile <$> fileArgument <*> textArgument
    cdBody = Cd <$> dirArgument
    rmBody = Rm <$> fileArgument
    rmdirBody = Rmdir <$> dirArgument
    findBody = Find <$> dirArgument <*> fileArgument
    dirBody = pure Dir