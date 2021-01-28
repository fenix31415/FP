{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE OverloadedStrings #-}

module FilesystemCore
  ( CommandExecutionError(..)
  , FSInfo(..) 
  , FileSystem
  , toAbsoluteFSPath
  , getFileByPath
  , getFileByPathOrError
  , getDirectoryByPathOrError
  , getDocumentByPathOrError
  , findByName
  , createFileByName
  , createFile
  , getAllFilesInSubDirectories
  , copyFile
  , removeFile
  ) where

import Control.Exception
import Control.Monad.Catch (throwM)
import Control.Monad.State
import Data.List.NonEmpty as NE
import SystemEntry
import Path
import Utils

{-|
  Structure representing filesystem.
  Exceptions are thrown via throwM so I assume StateT IO is fine.
-}
type FileSystem a = StateT FSInfo IO a

{-|
  Structure representing filesystem state.
-}
data FSInfo =
  FSInfo
    { currentDirectory :: SystemEntry    -- ^ current directory of user
    , rootDirectory :: SystemEntry       -- ^ root directory of filesystem
    , realRootPath :: StringPath  -- ^ root of a real filesystem
    }
  deriving (Show)

{-|
  Exception that may occur through commands execution
-}
data CommandExecutionError
  = DirectoryExpected
  | DocumentExpected
  | UnexpectedDocumentInPath
  | NoSuchFile
  | FileAlreadyExists
  | FileNotFound
  | FailedToCreateFile
  | CannotCreateRoot
  | CannotRemoveRoot
  | CannotRemoveParent
  | CVSAlreadyExists StringPath
  | CVSDoesNotExist
  | InvalidCVSRepository
  | InvalidCVSRevisionDirectory
  | FileNotAddedToCVS
  | MalformedCommitInfo
  | UnknownRevision
  deriving (Show, Exception)


-- Paths

{-|
  Convert given path to absolute path relative to the root of filesystem.
-}
toAbsoluteFSPath :: Path -> FileSystem Path
toAbsoluteFSPath path = do
  fsRoot <- filePath <$> gets rootDirectory
  currentDir <- filePath <$> gets currentDirectory
  return $ toAbsolutePath path fsRoot currentDir


-- Get file operations

{-|
  Safely retrieve file by given path.
  
  Returns @Maybe@ found file.
-}
getFileByPath :: Path -> FileSystem (Maybe SystemEntry)
getFileByPath path = toAbsoluteFSPath path >>= getFileByAbsolutePath

{-|
  Retrieve file by given path.
  
  Returns file if found.
  
  Throws:
    * @NoSuchFile@ if the file does not exist
-}
getFileByPathOrError :: Path -> FileSystem SystemEntry
getFileByPathOrError path = do
  maybeFile <- getFileByPath path
  case maybeFile of
    Just f -> return f
    Nothing -> throwM NoSuchFile

{-|
  Retrieve directory by given path.

  Returns @Directory@ instance if found.

  Throws:
    * @NoSuchFile@ if the file does not exits
    * @DirectoryExpected@ if the file is document
-}
getDirectoryByPathOrError :: Path -> FileSystem SystemEntry
getDirectoryByPathOrError path = do
  file <- getFileByPath path
  case file of
    Just dir@Directory{} -> return dir
    Just Document{} -> throwM DirectoryExpected
    Nothing -> throwM NoSuchFile

{-|
  Retrieve document by given path.

  Returns @Document@ instance if found.

  Throws:
    * @NoSuchFile@ if the file does not exits
    * @DocumentExpected@ if the file is document
-}
getDocumentByPathOrError :: Path -> FileSystem SystemEntry
getDocumentByPathOrError path = do
  file <- getFileByPath path
  case file of
    Just doc@Document{} -> return doc
    Just Directory{} -> throwM DocumentExpected
    Nothing -> throwM NoSuchFile

getFileByAbsolutePath :: Path -> FileSystem (Maybe SystemEntry)
getFileByAbsolutePath path = do
  root <- gets rootDirectory
  case path of
    ("/" :| []) -> return $ Just root
    ("/" :| remaining) -> getFileByRelativePath (NE.fromList remaining) root
    relativePath -> getFileByRelativePath relativePath root

getFileByRelativePath :: Path -> SystemEntry -> FileSystem (Maybe SystemEntry)
getFileByRelativePath _ Document{} = throw DirectoryExpected
getFileByRelativePath absPath@("/" :| _) _ = getFileByAbsolutePath absPath
getFileByRelativePath (x :| []) root = return $ findInFolder root x
getFileByRelativePath (x :| next : xs) root = do
  case findInFolder root x of
    Just f -> getFileByRelativePath (next :| xs) f
    Nothing -> return Nothing


-- Find operations

{-|
  Find documents with given name in given directory and its subdirectories.

  Returns list of found files. Files are guaranteed to be @Document@ instances.

  Throws the same errors as @getDirectoryByPathOrError@
-}
findByName :: Path -> String -> FileSystem [SystemEntry]
findByName path name = do
  root <- getDirectoryByPathOrError path
  return $ findInRootByNameRecursively root name

findInRootByNameRecursively :: SystemEntry -> String -> [SystemEntry]
findInRootByNameRecursively root name = do
  let initial = (:[]) <$> findInFolder root name `orElse` []
  foldr foldFunc initial (filterDirectories $ directoryContents root)
  where
    foldFunc dir acc = acc ++ findInRootByNameRecursively dir name

{-|
  Retrieve all files in the given directory recursively.
  
  Returns list of found @Document@ instances within given directory.
-}
getAllFilesInSubDirectories :: SystemEntry -> [SystemEntry]
getAllFilesInSubDirectories doc@Document{} = [doc]
getAllFilesInSubDirectories Directory{ directoryContents = contents } =
  concatMap getAllFilesInSubDirectories contents


-- SystemEntry creation operations

{-|
  Create file at given path with given name with possible overwriting.
  
  Returns created file.
  
  Throws the same errors as @createFile@
-}
createFileByName :: Path -> String -> SystemEntry -> Bool -> FileSystem SystemEntry
createFileByName parentPath name = createFile (parentPath <:| name)

{-|
  Create file at given path with possible overwriting.
  If some intermediate directories are missing, they are created too.
  
  Returns created file.
  
  Throws:
    * @FileAlreadyExists@ if file already exists at given path and overwriting is not enabled
    * @UnexpectedDocumentInPath@ if there is @Document@ file somewhere in the middle of the path
    * @CannotCreateRoot@ on attempt to pass "/" as path
    * @FailedToCreateFile@ on some unknown error
-}
createFile :: Path -> SystemEntry -> Bool -> FileSystem SystemEntry
createFile path newFile overwrite = do
  absPath <- toAbsoluteFSPath path
  root <- gets rootDirectory
  newRoot <- createFileRecursively absPath root newFile overwrite
  updateFileSystemWithNewRoot newRoot
  created <- getFileByPath absPath
  case created of
    Just file -> return file
    Nothing -> throwM FailedToCreateFile

createFileRecursively :: Path -> SystemEntry -> SystemEntry -> Bool -> FileSystem SystemEntry
createFileRecursively ("/" :| []) Directory{} _ _ = throwM CannotCreateRoot
createFileRecursively ("/" :| next) root@Directory{} newFile overwrite =
  createFileRecursively (NE.fromList next) root newFile overwrite
createFileRecursively (name :| []) root@Directory{ directoryContents = contents } newFile overwrite = do
  let fileWithUpdatedPaths = newFile {
      filePath = (filePath root) <:| name
    , fileParent = filePath root
    }
  let newRoot = root { directoryContents = contents ++ [fileWithUpdatedPaths] }
  case findInFolder root name of
    Just file ->
      if (not overwrite)
      then throwM FileAlreadyExists
      else updateFileInDirectory newRoot file newFile
    Nothing -> return $ newRoot
createFileRecursively path@(name :| next) root@Directory{} newFile overwrite =
  case findInFolder root name of
    Just dir@(Directory {}) -> do
      new <- createFileRecursively (NE.fromList next) dir newFile overwrite
      updateFileInDirectory root dir new
    Nothing -> do
      let newDirectory = emptyDirectory {
          filePath = (filePath root) <:| name
        , fileParent = filePath root
        }
      updatedRoot <- addToDirectory root newDirectory
      createFileRecursively path updatedRoot newFile overwrite
    Just (Document {}) -> throwM UnexpectedDocumentInPath
createFileRecursively _ Document{} _ _ = throwM UnexpectedDocumentInPath

updateFileSystemWithNewRoot :: SystemEntry -> FileSystem ()
updateFileSystemWithNewRoot newRoot = do
  modify (\s -> s { rootDirectory = newRoot })
  newCurrentDir <- gets currentDirectory >>= getDirectoryByPathOrError . filePath
  modify (\s -> s { currentDirectory = newCurrentDir })

addToDirectory :: SystemEntry -> SystemEntry -> FileSystem SystemEntry
addToDirectory dir@Directory{ directoryContents = contents } file =
  return dir { directoryContents = contents ++ [file] }
addToDirectory _ _ = throwM DirectoryExpected

updateFileInDirectory :: SystemEntry -> SystemEntry -> SystemEntry -> FileSystem SystemEntry
updateFileInDirectory dir@Directory{ directoryContents = contents } file newFile =
  return dir { directoryContents = update contents file newFile }
updateFileInDirectory _ _ _ = throwM DirectoryExpected


-- Copy operations

{-|
  Copy file to the path.
  
  Throws:
    * @FileAlreadyExists@ if file already exists at given path
    * @DirectoryExpected@ if there is @Document@ file somewhere in the middle of the path
    * @CannotCreateRoot@ on attempt to pass "/" as path
-}
copyFile :: SystemEntry -> Path -> FileSystem ()
copyFile file targetPath = do
  targetAbsPath <- toAbsoluteFSPath targetPath
  let newPath = targetAbsPath <:| (NE.last $ filePath file)
  root <- gets rootDirectory
  newRoot <- createFileRecursively newPath root file False
  updateFileSystemWithNewRoot newRoot
  case file of
    Directory{} -> updateFileSystemWithNewRoot $ updateParentsOfDirectoryContent newRoot
    Document{} -> return ()

updateParentsOfDirectoryContent :: SystemEntry -> SystemEntry
updateParentsOfDirectoryContent dir@Directory{ filePath = path, directoryContents = contents } =
  dir { directoryContents = Prelude.map (updateParents path) contents }
updateParentsOfDirectoryContent dir@Document{} = dir

updateParents :: Path -> SystemEntry -> SystemEntry
updateParents parentPath file@Document{}  =
  file
    { fileParent = parentPath
    , filePath = parentPath <:| (nameByPath . filePath $ file)
    }
updateParents parentPath file@Directory{ directoryContents = contents } = do
  let newPath = parentPath <:| (nameByPath . filePath $ file)
  file
    { fileParent = parentPath
    , filePath = newPath
    , directoryContents = Prelude.map (updateParents newPath) contents
    }


-- Removal operations

{-|
  Remove file located on the path.
  
  Throws:
    * @CannotRemoveParent@ on attempt to remove parent of current directory
    * @CannotRemoveRoot@ on attempt to remove root directory
    * @FileNotFound@ if there is no file found on the path
    * @UnexpectedDocumentInPath@ if there is @Document@ file somewhere in the middle of the path
-}
removeFile :: Path -> FileSystem ()
removeFile path = do
   absPath <- toAbsoluteFSPath path
   currentDirPath <- filePath <$> gets currentDirectory
   if absPath `Path.isParentOf` currentDirPath
     then throwM CannotRemoveParent
     else gets rootDirectory >>= removeFileRecursively absPath >>= updateFileSystemWithNewRoot

removeFileRecursively :: Path -> SystemEntry -> FileSystem SystemEntry
removeFileRecursively ("/" :| []) Directory{} = throwM CannotRemoveRoot
removeFileRecursively ("/" :| next) root@Directory{}  =
  removeFileRecursively (NE.fromList next) root
removeFileRecursively (name :| []) root@Directory{} =
  removeFromDirectory root <$> (findInFolder root name) `orElse` throwM FileNotFound
removeFileRecursively (name :| next) root@Directory{} =
  case findInFolder root name of
    Just dir@(Directory {}) -> do
      new <- removeFileRecursively (NE.fromList next) dir
      updateFileInDirectory root dir new
    Just Document{} -> throwM UnexpectedDocumentInPath
    Nothing -> throwM FileNotFound
removeFileRecursively _ Document{} = throwM UnexpectedDocumentInPath

removeFromDirectory :: SystemEntry -> SystemEntry -> FileSystem SystemEntry
removeFromDirectory dir@Directory{ directoryContents = contents } file =
  return dir { directoryContents = remove contents file }
removeFromDirectory _ _ = throwM DirectoryExpected


