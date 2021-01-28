{-# LANGUAGE OverloadedStrings #-}

module Main where

import Control.Monad.Catch (MonadCatch (catch))
import Control.Monad.State (MonadIO (liftIO), MonadTrans (lift), StateT (runStateT), gets)
import Control.Monad.Trans.Maybe (MaybeT (..))
import Data.ByteString.Lazy.Char8 (ByteString, pack, unpack)
import qualified Data.ByteString.Lazy.Char8 as BS (putStrLn)
import Data.List (intercalate)
import Data.List.Split (splitOn)
import FSIO (initFS, writeFS)
import FSOrerations (execAppend, execCD, execCP, execFind, execGetInfo, execLS, execMkdir, execRM,
                     execRead, execTouch)
import FSUtils (CommandExecutionError, FSState (..), FileSystem)
import Node (getNodeName)
import Options.Applicative (Parser, argument, execParser, fullDesc, header, help, helper, info,
                            metavar, progDesc, str, (<**>))
import Path (PathStr, getEntryName, showPathStr)
import System.IO (hFlush, stdout)

{-|
    A struct to represent arguments

    Arguments:
    * 'rootDir' -- a root directory of the FS
-}
newtype Args = Args {rootDir :: String}  -- maybe in the future will be 'data'

{-|
    Type of possible commands
-}
data Command
  = CDCommand PathStr               -- ^ 'cd' command
  | LSCommand PathStr               -- ^ 'ls' command
  | MKDirCommand PathStr            -- ^ 'mkDir' command
  | TouchCommand PathStr ByteString -- ^ 'touch' command
  | RMCommand PathStr               -- ^ 'rm' command
  | CatCommand PathStr              -- ^ 'cat' command
  | WriteCommand PathStr ByteString -- ^ 'append-to-file' command
  | CPCommand PathStr PathStr       -- ^ 'cp' command
  | InfoCommand PathStr             -- ^ 'get-info' command
  | FindCommand PathStr String      -- ^ 'find' command
  | ExitCommand                     -- ^ 'exit' command
  | HelpCommand                     -- ^ 'help' command
  | DontSaveCommand                 -- ^ 'quit-without-saving' command

{-|
    Error type, throwing if couldn't recognise command
-}
newtype UnknownCommandError = UnknownCommandError String deriving (Show)

{-|
    Parse agrs and execute @runFS@ with cpecified 'root' args
-}
main :: IO ()
main = do
  runFS =<< execParser opts
    where
      opts = info (argParser <**> helper)
        ( fullDesc
        <> progDesc "An atomic filesystem"
        <> header "Welcome!"
        )

{-|
    Parse agrs
-}
argParser :: Parser Args
argParser =
  Args <$> argument str (metavar "root" <> help "a root directory of the filesystem")

{-|
    A simple exception handler
-}
handleCmdExc :: CommandExecutionError -> FileSystem ()
handleCmdExc e = liftIO $ print e

{-|
    Reads and executes a user commands until the 'exit' command
-}
execLoop :: MaybeT (StateT FSState IO) ()
execLoop = do
  lift writeCurDir
  line <- liftIO getLine
  let command = getCommand line
  lift $ case command of
    Right cmd -> execCommand cmd `catch` handleCmdExc
    Left e    -> printError e
  case command of
    Right ExitCommand -> liftIO $ putStrLn "Saving fs"
    Right DontSaveCommand -> do
      liftIO $ putStrLn "Closing without saving"
      fail ""
    _ -> execLoop

{-|
    Init, run and, if needed, write fs on a disk
-}
runFS :: Args -> IO ()
runFS Args{ rootDir = root } = do
  start <- initFS root
  mbEnd <- runMaybeT $ do
    (mbVal, s) <- lift $ (runStateT $ runMaybeT execLoop) start
    case mbVal of
      Just _  -> lift $ return s
      Nothing -> MaybeT (return Nothing)
  mapM_ writeFS mbEnd

{-|
    Writes on a console current directory
-}
writeCurDir :: FileSystem ()
writeCurDir = do
  currentDir <- gets fsstateCurDir
  liftIO $ putStr $ show currentDir ++ "> "
  liftIO $ hFlush stdout

{-|
    Converts from @String@ to @Command@.
    If command is wrong -- returns an @UnknownCommandError@
-}
getCommand :: String -> Either UnknownCommandError Command
getCommand s =
  case splitOn " " s of
    ("cd" : [path])            -> Right $ CDCommand path
    ("ls" : [path])            -> Right $ LSCommand path
    ["ls"]                     -> Right $ LSCommand "."
    ("mkdir" : [path])         -> Right $ MKDirCommand path
    ("rm" : [path])            -> Right $ RMCommand path
    ("cp" : from : [to])       -> Right $ CPCommand from to
    ("touch" : path : [text])  -> Right $ TouchCommand path $ pack text
    ("touch" : [path])         -> Right $ TouchCommand path ""
    ("append" : path : [text]) -> Right $ WriteCommand path $ pack text
    ("cat" : [path])           -> Right $ CatCommand path
    ("info" : [path])          -> Right $ InfoCommand path
    ("find" : path : [name])   -> Right $ FindCommand path name
    ("find" : [name])          -> Right $ FindCommand "." name
    ["exit"]                   -> Right ExitCommand
    ["help"]                   -> Right HelpCommand
    ["q"]                      -> Right DontSaveCommand
    _                          -> Left $ UnknownCommandError s

{-|
    Print help message on a console
-}
execShowHelp :: FileSystem ()
execShowHelp = liftIO $ putStrLn $ intercalate "\n"
  [ "`cd <path>` -- change directory;"
  , "`ls [path]` -- show content"
  , "`mkdir <path>` -- create folder"
  , "`touch <path> <text>` -- create file"
  , "`cat <path>` -- show file text"
  , "`rm <path>` -- remove entry"
  , "`append <path> <text>` -- append to file"
  , "`find [path] <name>` -- find entry"
  , "`cp <path> <target-dir-path>` -- copy entry to path"
  , "`info <path>` -- show info of entry"
  , "`exit` -- exit with saving"
  , "`q` -- qq without saving"
  , "`help` -- help"
  ]

{-|
    Executes given command
-}
execCommand :: Command -> FileSystem ()
execCommand e =
  case e of
    CDCommand path -> execCD path
    LSCommand path -> do
      nodes <- execLS path
      let found = map getNodeName nodes
      liftIO $ case found of
        []      -> putStrLn "This dir is empty"
        [entry] -> print entry
        _       ->  print found
    MKDirCommand path -> do
      execMkdir path
      liftIO $ putStrLn $ "Directory '" ++ showPathStr path ++ "' succesfully created"
    TouchCommand path text -> do
      execTouch path text
      liftIO $ putStrLn $ if text == ""
        then "Empty file '" ++ showPathStr path ++ "' sucessfully created"
        else "file '" ++ showPathStr path ++ "' with text '" ++ take 50 (unpack text) ++ "' sucessfully created"
    CatCommand path -> do
      contents <- execRead path
      liftIO $ putStrLn $ "File: '" ++ showPathStr path ++ "', content: "
      liftIO $ BS.putStrLn contents
    RMCommand path -> do
      execRM path
      liftIO $ putStrLn $ "'" ++ showPathStr path ++ "' succesfully removed"
    WriteCommand path text -> do
      execAppend path text
      liftIO $ putStrLn $ "Sucessfully addded to file '" ++ showPathStr path ++ "' text '" ++ take 50 (unpack text) ++ "'"
      contents <- execRead path
      liftIO $ putStrLn $ "  Now file is: " ++ take 50 (unpack contents)
    FindCommand path name -> do
      result <- execFind path name
      liftIO $ case result of
        []      -> putStrLn "Nothing found"
        [entry] -> putStrLn $ "Found: " ++ show entry
        _       -> putStrLn $ "Found: " ++ show result
    CPCommand from to -> do
      execCP from to
      liftIO $ putStrLn $ "Succesfully copied from '" ++ showPathStr from ++ "' to '" ++ to ++ "/" ++ getEntryName from ++ "'"
    InfoCommand path -> do
      t <- execGetInfo path
      liftIO $ putStrLn t
    HelpCommand -> execShowHelp
    _ -> return ()  -- for q() & exit(), ignore here

{-|
    Print an error message on a console
-}
printError :: UnknownCommandError -> FileSystem ()
printError err = liftIO $ putStrLn $ "Wrong command: " ++ show err ++ ". Usage: help"
