{-# LANGUAGE OverloadedStrings #-}
 
module Main where

import Control.Monad.Catch ( MonadCatch(catch) )
import Control.Monad.State ( gets, execStateT, MonadIO(liftIO) )
import qualified Data.ByteString.Lazy.Char8 as BS
import Data.List ( intercalate )
import SystemEntry ( fileName )
import FilesystemCommands
    ( appendToFile,
      changeDirectory,
      copyFile,
      findByName,
      getContents,
      getFileInfo,
      makeDirectory,
      makeFile,
      readFileContents,
      removeFile )
import FilesystemCore
  ( CommandExecutionError
  , FSInfo(..)
  , FileSystem
    )
import FilesystemDumper
import FilesystemLoader
import Options.Applicative hiding (command)
import Path
import System.IO (hFlush, stdout)
import Utils
import Data.List.Split

data Args = Args { rootDir :: String }

data Command
  = ChangeDirectory StringPath
  | ListFiles StringPath
  | MakeDirectory StringPath
  | MakeDocument StringPath BS.ByteString
  | RemoveFile StringPath
  | ReadFile StringPath
  | AppendFile StringPath BS.ByteString
  | Copy StringPath StringPath
  | PrintInfo StringPath
  | Find StringPath String
  | Exit
  | ShowHelp

data UnknownCommandError
  = UnknownCommandError String
  | IntParseError String
  | UnknownMergeStrategy String
  deriving (Show)

{-|
  The main entry point for the program.
  Takes path to root directory from where to launch file manager as required argument.
-}
main :: IO ()
main = do
  runFS =<< execParser opts
    where
      opts = info (argParser <**> helper)
        ( fullDesc
        <> progDesc "progDesc"
        <> header "header"
        )

argParser :: Parser Args
argParser =
  Args <$> argument str (metavar "metavar" <> help "help")

{-|
  Load filesystem in memory, execute user commands and dump resulting filesystem to the
  real PC filesystem.
-}
runFS :: Args -> IO ()
runFS Args{ rootDir = root } = do
  start <- initFS root
  end <- execStateT prompt start
  dumpFilesystem end

{-|
  Endless loop waiting for user to prompt and executing given commands
-}
prompt :: FileSystem ()
prompt = do
  currentDir <- gets currentDirectory
  liftIO $ putStr $ show currentDir ++ "> "
  liftIO $ hFlush stdout
  line <- liftIO getLine
  let command = parseCommand line
  case command of
    Right cmd -> execCommand cmd `catch` printCommandExecutionError
    Left err -> printError err
  case command of
    Right Exit -> return ()
    _ -> prompt

parseCommand :: String -> Either UnknownCommandError Command
parseCommand s =
  case splitOn " " s of
    ("cd" : [path]) -> Right $ ChangeDirectory path
    ("ls" : [path]) -> Right $ ListFiles path
    ("mkdir" : [path]) -> Right $ MakeDirectory path
    ("rm" : [path]) -> Right $ RemoveFile path
    ("cp" : from : [to]) -> Right $ Copy from to
    ("touch" : path : [text]) -> Right $ MakeDocument path $ BS.pack text
    ("touch" : [path]) -> Right $ MakeDocument path ""
    ("append-file" : path : [text]) -> Right $ AppendFile path $ BS.pack text
    ("cat" : [path]) -> Right $ ReadFile path
    ("info" : [path]) -> Right $ PrintInfo path
    ("find" : path : [name]) -> Right $ Find path name
    ["exit"] -> Right Exit
    ["help"] -> Right ShowHelp
    _ -> Left $ UnknownCommandError s

execCommand :: Command -> FileSystem ()
execCommand e =
  case e of
    ChangeDirectory path -> execCd path
    ListFiles path -> execLs path
    MakeDirectory path -> execMkdir path
    MakeDocument path text -> execTouch path text
    RemoveFile path -> execRm path
    Copy from to -> execCp from to
    ReadFile path -> execReadFile path
    AppendFile path text -> execAppendFile path text
    PrintInfo path -> execPrintInfo path
    Find path name -> execFind path name
    Exit -> execExit
    ShowHelp -> execShowHelp

execCd :: StringPath -> FileSystem ()
execCd = changeDirectory

execLs :: StringPath -> FileSystem ()
execLs path = do
  contents <- FilesystemCommands.getContents path
  liftIO $ print $ Prelude.map fileName contents

execMkdir :: StringPath -> FileSystem ()
execMkdir = makeDirectory

execTouch :: StringPath -> BS.ByteString -> FileSystem ()
execTouch = makeFile

execRm :: StringPath -> FileSystem ()
execRm = removeFile

execCp :: StringPath -> StringPath -> FileSystem ()
execCp = copyFile

execReadFile :: StringPath -> FileSystem ()
execReadFile path = do
  contents <- readFileContents path
  liftIO $ BS.putStrLn contents

execAppendFile :: StringPath -> BS.ByteString -> FileSystem ()
execAppendFile = appendToFile

execPrintInfo :: StringPath -> FileSystem ()
execPrintInfo path = getFileInfo path >>= liftIO . putStrLn

execFind :: StringPath -> String -> FileSystem ()
execFind path name = do
  result <- findByName path name
  liftIO $ print result

execExit :: FileSystem ()
execExit = do
  liftIO $ putStrLn "Dumping files on hard drive..."
  liftIO $ putStrLn "Backup will be created in .old_fs directory"

execShowHelp :: FileSystem ()
execShowHelp = liftIO $ putStrLn getHelp

printCommandExecutionError :: CommandExecutionError -> FileSystem ()
printCommandExecutionError e = liftIO $ print e

printError :: UnknownCommandError -> FileSystem ()
printError err = liftIO $ putStrLn $ "Wrong command: " ++ show err ++ ". Usage: help"

tryParse :: String -> Either UnknownCommandError Int
tryParse s =
  case readMaybeInt s of
    Just i -> Right i
    Nothing -> Left $ IntParseError s

getHelp :: String
getHelp = intercalate "\n"
  [ "`cd <path>` -- перейти в указанный путь;"
  , "`ls <path>` -- показать содержимое текущей директории;"
  , "`mkdir <path>` -- создать директорию, включая промежуточные;"
  , "`touch <path> <text>` -- создать файл, включая промежуточные директории;"
  , "`cat <path>` -- отобразить содержимое файла;"
  , "`rm <path>` -- удалить папку/файл;"
  , "`cp <path> <target-dir-path>` -- скопировать папку/файл в указанную директорию с созданием промежуточных;"
  , "`append-file <path> <text>` -- записать в файл текстовую информацию;"
  , "`find <path> <name>` -- поиск файла по названию в указанной директории и ее подчастях и вывод пути до файла;"
  , "`info <path>` -- отобразить информацию о заданном файле;"
  , "`info <path>` -- отобразить информацию о директории;"
  , "`exit` -- завершить работу с менеджером и выгрузить изменения на диск;"
  , "`help` -- показать это сообщение."
  ]
