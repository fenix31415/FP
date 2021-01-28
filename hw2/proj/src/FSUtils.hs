{-# LANGUAGE OverloadedStrings #-}

module FSUtils
  ( CommandExecutionError(..)
  , FSState(..)
  , FileSystem
  , toAbsoluteFSPath
  , mbFindNode
  , getDir
  , getDoc
  , findNode
  , createFile
  , createFileByName
  , createFile_
  , copyFile
  , removeFile
  , getDocs
  ) where

import Control.Exception (Exception)
import Control.Monad.Catch (throwM)
import Control.Monad.State (StateT, gets, modify, void)
import Data.List.NonEmpty as NE (NonEmpty ((:|)), fromList, nonEmpty)
import Data.Maybe (fromJust, maybeToList)
import Node (Node (Directory, Document, nodeChildrens, nodeParent, nodePath), emptyDirFactory,
             findInFolder, getOnlyDirs)
import Path (Path, PathStr, isParent, nameByPath, showPath, toAbsolutePath)
import Utils (nonAppendEl, removeFromList, updateList)

{-|
  Structure representing filesystem.
  Exceptions are thrown via throwM so I assume StateT IO is fine.
-}
type FileSystem a = StateT FSState IO a -- i promise i will use throwM, so it should work..

{-|
  Structure representing filesystem state.
-}
data FSState =
  FSState
    { fsstateRoot    :: Node    -- ^ root directory of filesystem
    , fsstateCurDir  :: Node    -- ^ current directory of user
    , fsstateAbsRoot :: PathStr -- ^ root of a real filesystem
    }
  deriving (Show)

{-|
  Exception that may occur through commands execution
-}
data CommandExecutionError
  = DirectoryExpected Path    -- ^ directory is document
  | DocumentExpected Path     -- ^ document is directory
  | DocInPath Path            -- ^ document middle of path
  | NoSuchFile String         -- ^ file does not exits
  | AlreadyExists String      -- ^ on attempt to overwrite with disabled feature
  | FailedToCreateFile Path   -- ^ some error occur, possibly connected with OS
  | OverwritingRoot           -- ^ on attempt to overwrite root of fs
  | RemovingRoot              -- ^ on attempt to remove root of fs
  | RemovingParent            -- ^ on attempt to remove parent of current directory

instance Show CommandExecutionError where
  show (DirectoryExpected path) = "Expected directory, but file found at: '" ++ showPath path ++ "'"
  show (DocumentExpected path) = "Expected document, but directory found at: '" ++ showPath path ++ "'"
  show (DocInPath path) = "Unexpected document in path: '" ++ showPath path ++ "'"
  show (NoSuchFile path) = "No file found: '" ++ path ++ "'"
  show (AlreadyExists path) = "File: '" ++ path ++ "' already exists"
  show (FailedToCreateFile path) = "Failed to create file '" ++ showPath path ++ "'"
  show OverwritingRoot = "Attempt to overwrite root"
  show RemovingRoot = "Attempt to remove root"
  show RemovingParent = "Attempt to remove parent of cd"

instance Exception CommandExecutionError

{-|
    Converts given path to absolute path relative to the root of filesystem.
-}
toAbsoluteFSPath :: Path -> FileSystem Path
toAbsoluteFSPath path = do
  fsRoot <- gets (nodePath . fsstateRoot)
  currentDir <- gets (nodePath . fsstateCurDir)
  return $ toAbsolutePath path fsRoot currentDir

{-|
    Returns @Maybe@ entry by relative path.
-}
mbFindNode :: Path -> FileSystem (Maybe Node)
mbFindNode relPath = do
  absPath <- toAbsoluteFSPath relPath
  mbGetNodeAbs absPath

{-|
    Returns @Maybe@ entry by abcolute path.
-}
mbGetNodeAbs :: Path -> FileSystem (Maybe Node)
mbGetNodeAbs absPath = do
  root <- gets fsstateRoot
  case absPath of
    ("/" :| []) -> return $ Just root
    ("/" :| p)  -> mbGetNodeRelRec (fromList p) root
    rel         -> mbGetNodeRelRec rel root

{-|
    Returns @Maybe@ entry by relative path of the given node
-}
mbGetNodeRelRec :: Path -> Node -> FileSystem (Maybe Node)
mbGetNodeRelRec path Document{} = throwM $ DirectoryExpected path
mbGetNodeRelRec absPath@("/" :| _) _ = mbGetNodeAbs absPath
mbGetNodeRelRec (x :| []) root = return $ findInFolder root x
mbGetNodeRelRec (x :| xs) root =
  case findInFolder root x of
    Nothing     -> return Nothing
    Just folder -> mbGetNodeRelRec (fromJust $ nonEmpty xs) folder

{-|
    Returns @Directory@ instance if found.

    Throws:
    * @NoSuchFile@ if the directory does not exits
    * @DirectoryExpected@ if the file is document
-}
getDir :: Path -> FileSystem Node
getDir path = do
  file <- mbFindNode path
  case file of
    Nothing              -> throwM $ NoSuchFile $ showPath path
    Just Document{}      -> throwM $ DirectoryExpected path
    Just dir@Directory{} -> return dir

{-|
    Returns @Document@ instance if found.

    Throws:
    * @NoSuchFile@ if the directory does not exits
    * @DocumentExpected@ if the file is document
-}
getDoc :: Path -> FileSystem Node
getDoc path = do
  file <- mbFindNode path
  case file of
    Nothing             -> throwM $ NoSuchFile $ showPath path
    Just Directory{}    -> throwM $ DocumentExpected path
    Just doc@Document{} -> return doc

{-|
  Find documents with given name in given directory and its subdirectories.

  Returns list of found files. Files are guaranteed to be @Document@ instances.

  Throws the same errors as @getDir@
-}
findNode :: Path -> String -> FileSystem [Node]
findNode start name = do
  root <- getDir start
  return $ findNode_ root name where
    findNode_ :: Node -> String -> [Node]
    findNode_ root' name' = do
      let initial = maybeToList (findInFolder root' name')
      foldr f initial (getOnlyDirs $ nodeChildrens root')
      where
        f dir acc = acc ++ findNode_ dir name

{-|
  Create file at the given path with possible overwriting, with subdirectories

  Returns created file.

  Throws:
  * @AlreadyExists@ if file already exists and overwriting is disabled
  * @DocInPath@ if document in path
  * @OverwritingRoot@ if "/" as path
  * @FailedToCreateFile@ on some unknown error (permissions etc)
-}
createFile :: Path -> Node -> Bool -> FileSystem Node
createFile path newFile overwrite = do
  absPath <- toAbsoluteFSPath path
  root <- gets fsstateRoot
  newRoot <- createFileHelper absPath root newFile overwrite
  updateFS newRoot
  created <- mbFindNode absPath
  case created of
    Just file -> return file
    Nothing   -> throwM $ FailedToCreateFile path

{-|
    Updates fs.
-}
updateFS :: Node -> FileSystem ()
updateFS newRoot = do
  modify (\s -> s { fsstateRoot = newRoot })
  newCurrentDir <- gets fsstateCurDir >>= getDir . nodePath
  modify (\s -> s { fsstateCurDir = newCurrentDir })

{-|
    Create file at given path with possible overwriting, create subdirectories

    Returns created file.

    Throws: same as @createFile@
-}
createFile_ :: Path -> Node -> Bool -> FileSystem ()
createFile_ p n o = void $ createFile p n o

createFileHelper :: Path -> Node -> Node -> Bool -> FileSystem Node
createFileHelper _ Document{ nodePath=path } _ _ = throwM $ DocInPath path
createFileHelper ("/" :| []) Directory{} _ _ = throwM OverwritingRoot
createFileHelper ("/" :| next) root@Directory{} newFile overwrite =
  createFileHelper (fromList next) root newFile overwrite
createFileHelper (name :| []) root@Directory{ nodeChildrens = childs } newFile overwrite = do
  let rootPath = nodePath root
  let fileWithUpdatedPaths = newFile {
      nodePath = rootPath `nonAppendEl` name
    , nodeParent = rootPath
    }
  let newRoot = root { nodeChildrens = childs ++ [fileWithUpdatedPaths] }
  case findInFolder root name of
    Just file ->
      if not overwrite
      then throwM $ AlreadyExists name
      else rewriteFileInDir newRoot file newFile
    Nothing -> return newRoot
createFileHelper path@(name :| next) root@Directory{} newFile overwrite =
  case findInFolder root name of
    Just dirOrDoc -> do
      new <- createFileHelper (fromList next) dirOrDoc newFile overwrite
      rewriteFileInDir root dirOrDoc new
    Nothing -> do
      let rootPath = nodePath root
      let newDirectory = emptyDirFactory {
          nodePath = rootPath `nonAppendEl` name
        , nodeParent = rootPath
        }
      updatedRoot <- addToDirectory root newDirectory
      createFileHelper path updatedRoot newFile overwrite

{-|
  Create file at the given path with possible overwriting, with subdirectories

  Returns created file.

  Throws:
  * @AlreadyExists@ if file already exists and overwriting is disabled
  * @DocInPath@ if document in path
  * @OverwritingRoot@ if "/" as path
  * @FailedToCreateFile@ on some unknown error (permissions etc)
-}
createFileByName :: Path -> String -> Node -> Bool -> FileSystem Node
createFileByName parentPath name =
  createFile $ parentPath `nonAppendEl` name

{-|
    Adds a 'node' to the given 'dir'

    Returns updated 'dir'

    Throws:
    * @DirectoryExpected@ if 'dir' is document
-}
addToDirectory :: Node -> Node -> FileSystem Node
addToDirectory dir@Directory{ nodeChildrens = contents } node =
  return dir { nodeChildrens = contents ++ [node] }
addToDirectory doc _ = throwM $ DirectoryExpected (nodePath doc)

{-|
    Rewrites `file` in given `dir` to `newFile`.

    Throws:
    * @DirectoryExpected@ if 'dir' is document
-}
rewriteFileInDir :: Node -> Node -> Node -> FileSystem Node
rewriteFileInDir dir@Directory{ nodeChildrens = contents } file newFile = return dir { nodeChildrens = updateList contents file newFile }
rewriteFileInDir doc _ _ = throwM $ DirectoryExpected (nodePath doc)

{-|
    Copy entry to the path.

    Throws:
    * @AlreadyExists@ if file already exists
    * @DocInPath@ if document in path
    * @OverwritingRoot@ if "/" as path
-}
copyFile :: Node -> Path -> FileSystem ()
copyFile file targetPath = do
  targetAbsPath <- toAbsoluteFSPath targetPath
  let newPath = targetAbsPath `nonAppendEl` nameByPath (nodePath file)
  root <- gets fsstateRoot
  newRoot <- createFileHelper newPath root file False
  updateFS newRoot
  case file of
    Directory{} -> updateFS $ updateParentChildrens newRoot
    Document{}  -> return ()

{-|
    Returns list of found @Document@ instances in the given directory.
-}
getDocs :: Node -> [Node]
getDocs doc@Document{} = [doc]
getDocs Directory{ nodeChildrens = contents } =
  concatMap getDocs contents

{-|
    Updates @nodeParent@ and @nodePath@ fields of all children of the given node.
-}
updateParentChildrens :: Node -> Node
updateParentChildrens dir@Directory{ nodePath = path, nodeChildrens = contents } =
  dir { nodeChildrens = Prelude.map (updateParents path) contents }
updateParentChildrens doc@Document{} = doc

{-|
    Updates @nodeParent@ and @nodePath@ fields of the given node.
-}
updateParents :: Path -> Node -> Node
updateParents parentPath doc@Document{} =
  doc
    { nodeParent = parentPath
    , nodePath = parentPath `nonAppendEl` (nameByPath . nodePath $ doc)
    }
updateParents parentPath dir@Directory{ nodeChildrens = contents } = do
  let newPath = parentPath `nonAppendEl` (nameByPath . nodePath $ dir)
  dir
    { nodeParent = parentPath
    , nodePath = newPath
    , nodeChildrens = Prelude.map (updateParents newPath) contents
    }

{-|
    Remove file located on the path.

    Throws:
    * @RemovingParent@ if removing parent of current directory
    * @RemovingRoot@ if removing root
    * @NoSuchFile@ if there is no file on the path
    * @DocInPath@ if document in path
-}
removeFile :: Path -> FileSystem ()
removeFile path = do
  absPath <- toAbsoluteFSPath path
  currentDirPath <- gets (nodePath . fsstateCurDir)
  let action | absPath `isParent` currentDirPath = throwM RemovingParent
             | absPath == ("/" :| []) = throwM RemovingRoot
             | otherwise = do
               t <- gets fsstateRoot
               q <- removeFileHelper absPath t
               updateFS q
  action

removeFileHelper :: Path -> Node -> FileSystem Node
removeFileHelper ("/" :| []) Directory{} = throwM RemovingRoot
removeFileHelper ("/" :| next) root@Directory{}  = removeFileHelper (NE.fromList next) root
removeFileHelper (name :| []) root@Directory{} = maybe (throwM $ NoSuchFile name) (removeFromDir root) (findInFolder root name)
removeFileHelper (name :| next) root@Directory{} =
  case findInFolder root name of
    Just dir@Directory {} -> do
      new <- removeFileHelper (NE.fromList next) dir
      rewriteFileInDir root dir new
    Just Document{ nodePath=path } -> throwM $ DocInPath path
    Nothing -> throwM $ NoSuchFile name
removeFileHelper _ Document{ nodePath=path } = throwM $ DocInPath path

{-|
    Remove entry if it child of the given directory.

    Throws:
    * @DirectoryExpected@ given entry is document
-}
removeFromDir :: Node -> Node -> FileSystem Node
removeFromDir dir@Directory{ nodeChildrens = childrens } file = return dir { nodeChildrens = removeFromList childrens file }
removeFromDir doc _ = throwM $ DirectoryExpected (nodePath doc)
