module FSIO ( initFS, writeFS ) where

import Control.Applicative ((<|>))
import Control.Monad (guard)
import Control.Monad.IO.Class (liftIO)
import Control.Monad.Trans.Maybe (MaybeT (runMaybeT))
import qualified Data.ByteString.Lazy as BS
import Data.ByteString.Lazy.Char8 (unpack)
import Data.List.NonEmpty as NE (NonEmpty ((:|)))
import Data.Maybe (catMaybes, fromJust)
import FSUtils (FSState (..))
import Node (Node (..))
import Path (PathStr, toPath, toStr, (</>))
import System.Directory (createDirectoryIfMissing, doesDirectoryExist, doesFileExist,
                         getCurrentDirectory, getModificationTime, getPermissions, listDirectory,
                         makeAbsolute, setCurrentDirectory, withCurrentDirectory)


------------------------------------------------------------
--- I
------------------------------------------------------------

{-|
    Returns a new filesystem
-}
initFS :: String -> IO FSState
initFS root = do
  absRoot <- makeAbsolute root
  rootDir <- fromJust <$> runMaybeT (dirNodeFactory "/" absRoot)
  return FSState
    { fsstateCurDir = rootDir
    , fsstateRoot = rootDir
    , fsstateAbsRoot = absRoot
    }

{-|
    Computes path by parent and local paths
-}
getRealPath :: FilePath -> FilePath -> FilePath
getRealPath parentPath path =
  if parentPath == "/"
    then "/" ++ path
    else parentPath </> path

{-|
    Returns a real folder contents, creating for each item a @Node@
-}
getFolderContents :: FilePath -> FilePath -> FilePath -> IO [Node]
getFolderContents absFsRoot realPath localPath = do
  withCurrentDirectory realPath $ do
    currentDir <- getCurrentDirectory
    paths <- listDirectory currentDir
    catMaybes <$> traverse (runMaybeT . getNode absFsRoot localPath) paths where
      getNode absFsRoot' parentPath' path' =
        createDocNode parentPath' path' <|>
        dirNodeFactory (getRealPath parentPath' path') absFsRoot'

{-|
    Returns a @Directory@ by path if it is present
-}
dirNodeFactory :: PathStr -> FilePath -> MaybeT IO Node
dirNodeFactory localPath absFsRoot= do
  let absPath = absFsRoot ++ localPath
  isDirectory <- liftIO $ doesDirectoryExist absPath
  guard isDirectory
  children <- liftIO $ map (\n -> n { nodeParent = toPath localPath }) <$> getFolderContents absFsRoot absPath localPath
  permissions <- liftIO $ getPermissions absPath
  return Directory
    { nodePath = toPath localPath
    , nodeParent = toPath "/"
    , nodePermissions = permissions
    , nodeChildrens = children
    }

{-|
    Returns a @Document@ by path if it is present
-}
createDocNode :: FilePath -> FilePath -> MaybeT IO Node
createDocNode localParentPath path = do
  let realPath = getRealPath localParentPath path
  isFile <- liftIO $ doesFileExist path
  guard isFile
  permissions <- liftIO $ getPermissions path
  contents <- liftIO $ BS.readFile path
  modificationTime <- liftIO $ getModificationTime path
  return Document
    { nodePath = toPath realPath
    , nodeParent = toPath "/"
    , nodePermissions = permissions
    , docContent = contents
    , docCreationTime = modificationTime
    , docUpdateTime = modificationTime
    }

------------------------------------------------------------
--- O
------------------------------------------------------------

{-|
    Writes all FS into the disk
-}
writeFS :: FSState -> IO ()
writeFS fsState = do
  let rootPath = fsstateAbsRoot fsState
  setCurrentDirectory rootPath
  rmrf rootPath
  writeNode (fsstateRoot fsState) rootPath
  return ()

{-|
    Recursively removes all entries from the given path
-}
rmrf ::  FilePath -> IO ()
rmrf path = do
  files <- listDirectory path
  mapM_ (\name -> rmrf (path </> name)) files

{-|
    Recursively writes a given @Node@ (Dir or Doc) to the given path
-}
writeNode :: Node -> FilePath -> IO ()
writeNode node absFsRoot = let
  toAbsPath path absFsRoot' =
    case path of
      "/" :| []     -> absFsRoot'
      "/" :| [x]    -> absFsRoot' </> x
      "/" :| x : xs -> absFsRoot' </> toStr (x :| xs)
      _             -> error "invalid path"
  in case node of
    Document{ nodePath = path, docContent = contents } -> do
      writeFile (toAbsPath path absFsRoot) (unpack contents)
    Directory{ nodePath = path, nodeChildrens = contents } -> do
      createDirectoryIfMissing False (toAbsPath path absFsRoot)
      mapM_ (`writeNode` absFsRoot) contents

