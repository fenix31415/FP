{-# LANGUAGE OverloadedStrings #-}

module FilesystemLoader ( initFS ) where

import Control.Applicative ((<|>))
import Control.Monad
import Control.Monad.IO.Class (liftIO)
import Control.Monad.Trans.Maybe
import qualified Data.ByteString.Lazy as BS
import Data.Maybe (catMaybes, fromJust)
import SystemEntry
import FilesystemCore
import Path
import System.Directory

{-|
  Load filesystem by root path and return constructed from it @FSState@ .
-}
initFS :: String -> IO FSInfo
initFS root = do
  absRoot <- makeAbsolute root
  rootDir <- fromJust <$> runMaybeT (constructDirectory absRoot "" "/")
  return FSInfo
    { currentDirectory = rootDir
    , rootDirectory = rootDir
    , realRootPath = absRoot
    }

getFolderContents :: FilePath -> FilePath -> FilePath -> IO [SystemEntry]
getFolderContents realFsRoot realPath localPath = do
  withCurrentDirectory realPath $ do
    currentDir <- getCurrentDirectory
    paths <- listDirectory currentDir
    catMaybes <$> traverse (runMaybeT . fileFromPath realFsRoot localPath) paths

fileFromPath :: FilePath -> FilePath -> FilePath -> MaybeT IO SystemEntry
fileFromPath realFsRoot parentPath path =
  constructDocument parentPath path <|> constructDirectory realFsRoot parentPath path

constructDirectory :: FilePath -> FilePath -> FilePath -> MaybeT IO SystemEntry
constructDirectory realFsRoot localParentPath name = do
  let localPath =
        case localParentPath of
          "" -> "/"
          "/" -> "/" ++ name
          _ -> localParentPath </> name
  let realPath = realFsRoot ++ localPath

  isDirectory <- liftIO $ doesDirectoryExist realPath
  guard isDirectory

  let updateFunction = \file -> file { fileParent = stringToPath localPath }
  contents <- liftIO $ map updateFunction <$> getFolderContents realFsRoot realPath localPath
  permissions <- liftIO $ getPermissions realPath

  return Directory
    { filePath = stringToPath localPath
    , filePermissions = permissions
    , directoryContents = contents
    , fileParent = stringToPath "/"
    }

constructDocument :: FilePath -> FilePath -> MaybeT IO SystemEntry
constructDocument parentPath path = do
  let realPath =
        if parentPath == "/"
          then "/" ++ path
          else parentPath </> path
  isFile <- liftIO $ doesFileExist path
  guard isFile

  let filepath = stringToPath realPath
  permissions <- liftIO $ getPermissions path
--  creationTime <- liftIO $ getCreationTime path          couldn't find it in directory lib :(
  modificationTime <- liftIO $ getModificationTime path
  contents <- liftIO $ BS.readFile path

  return Document
    { filePath = filepath
    , filePermissions = permissions
    , fileParent = stringToPath "/"
    , documentCreationTime = modificationTime
    , documentUpdateTime = modificationTime
    , documentContent = contents
    }
