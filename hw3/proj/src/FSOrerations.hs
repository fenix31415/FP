module FSOrerations
  ( execAppend
  , execCD
  , execCP
  , execFind
  , execGetInfo
  , execLS
  , execMkdir
  , execRM
  , execRead
  , execTouch
  ) where

import Control.Monad.Catch (throwM)
import Control.Monad.State (MonadIO (liftIO), MonadState (get), evalStateT, modify)
import Data.ByteString.Lazy (ByteString)
import qualified Data.ByteString.Lazy as BS (length)
import Data.Time (UTCTime)
import Data.Time.Clock.System (getSystemTime, systemToUTCTime)
import FSUtils (CommandExecutionError (NoSuchFile), FSState (fsstateCurDir), FileSystem, copyFile,
                createFile_, findNode, getDir, getDoc, mbFindNode, removeFile)
import GHC.Int (Int64)
import Node (Node (..), emptyDirFactory, emptyDocFactory)
import Path (PathStr, getExtension, toPath, toStr)
import System.Directory (Permissions)

{-|
    Changes current directory of the fs
-}
execCD :: PathStr -> FileSystem ()
execCD path = do
  curSt <- get
  newDir <- liftIO $ evalStateT (getDir $ toPath path) curSt
  modify (\s -> s { fsstateCurDir = newDir })

{-|
    Returns a contents of directory located in the given path
-}
execLS :: PathStr -> FileSystem [Node]
execLS path = do
  dir <- getDir $ toPath path
  return $ nodeChildrens dir

{-|
    Creates a new directory by the given path
-}
execMkdir :: PathStr -> FileSystem ()
execMkdir stringPath = do
  let path = toPath stringPath
  createFile_ path emptyDirFactory False

{-|
    Creates a new file by the given path
-}
execTouch :: PathStr -> ByteString -> FileSystem ()
execTouch stringPath text = do
  let path = toPath stringPath
  modTime <- liftIO $ systemToUTCTime <$> getSystemTime
  let node = (emptyDocFactory modTime) { docContent = text }
  createFile_ path node False

{-|
    Removes an entry from the FS
-}
execRM :: PathStr -> FileSystem ()
execRM = removeFile . toPath

{-|
    Copy an entry from the FS
-}
execCP :: PathStr -> PathStr -> FileSystem ()
execCP path targetPath = do
  mbNode <- mbFindNode $ toPath path
  case mbNode of
    Just f  -> copyFile f (toPath targetPath)
    Nothing -> throwM $ NoSuchFile path

{-|
    Adds given text in the given file
-}
execAppend :: PathStr -> ByteString -> FileSystem ()
execAppend path text = do
  file <- getDoc $ toPath path
  modTime <- liftIO $ systemToUTCTime <$> getSystemTime
  let newFile = file {
    docUpdateTime = modTime,
    docContent = docContent file <> text
  }
  createFile_ (toPath path) newFile True

{-|
    Returns contents of the given file
-}
execRead :: PathStr -> FileSystem ByteString
execRead path = do
  file <- getDoc $ toPath path
  return $ docContent file

{-|
    Returns a @String@ representing all info of the file/folder
-}
execGetInfo :: PathStr -> FileSystem String
execGetInfo path = do
  file <- mbFindNode $ toPath path
  case file of
    Just dir@Directory{} -> do
      let filepath = toStr $ nodePath dir
      let parentPathStr = show $ toStr $ nodeParent dir
      let permissions = nodePermissions dir
      let fileCount = Prelude.length $ nodeChildrens dir
      let size = getNodeSize dir
      return $ getDirInfo filepath parentPathStr permissions fileCount size
    Just doc@Document{} -> do
      let filepath = toStr $ nodePath doc
      let parentPathStr = show $ toStr $ nodeParent doc
      let permissions = nodePermissions doc
      let extension = getExtension $ nodePath doc
      let creationTime = docCreationTime doc
      let modificationTime = docUpdateTime doc
      let size = getNodeSize doc
      return $ getDocInfo filepath parentPathStr permissions extension creationTime modificationTime size
    Nothing -> throwM $ NoSuchFile path

{-|
    Constructs a @String@ representing all info of the folder
-}
getDirInfo :: PathStr -> PathStr -> Permissions -> Int -> Int64 -> String
getDirInfo path parentPath permissions fileCount size =
  "Path: " ++ path ++ "\n" ++
  "Parent: " ++ parentPath ++ "\n" ++
  "Permissions: " ++ show permissions ++ "\n" ++
  "Files: " ++ show fileCount ++ "\n" ++
  "Size: " ++ show size ++ "B"

{-|
    Constructs a @String@ representing all info of the file
-}
getDocInfo :: PathStr -> PathStr -> Permissions -> String -> UTCTime -> UTCTime -> Int64 -> String
getDocInfo path parentPath permissions extension creationTime modificationTime size =
  "Path: " ++ path ++ "\n" ++
  "Parent: " ++ parentPath ++ "\n" ++
  "Permissions: " ++ show permissions ++ "\n" ++
  "File type: " ++ extension ++ "\n" ++
  "Created at: " ++ show creationTime ++ "\n" ++
  "Updated at: " ++ show modificationTime ++ "\n" ++
  "Size: " ++ show size ++ "B"

{-|
    Constructs a @String@ representing all info of the file
-}
getNodeSize :: Node -> Int64
getNodeSize dir@Directory{} = foldr ((+) . getNodeSize) 0 (nodeChildrens dir)
getNodeSize doc@Document{}  = BS.length $ docContent doc

{-|
    Returns a 'list' of files containing given string
-}
execFind :: PathStr -> String -> FileSystem [String]
execFind rootPath name = do
  files <- FSUtils.findNode (toPath rootPath) name
  return $ map (toStr . nodePath) files
