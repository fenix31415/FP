module FilesystemCommands where

import Control.Monad.Catch (throwM)
import Control.Monad.State
import qualified Data.ByteString.Lazy as BS
import Data.Time (UTCTime)
import Data.Time.Clock.System (getSystemTime, systemToUTCTime)
import SystemEntry
import FilesystemCore
import GHC.Int (Int64)
import Path
import System.Directory (Permissions)

changeDirectory :: StringPath -> FileSystem ()
changeDirectory path = do
  st <- get
  newDirectory <- liftIO $ evalStateT (getDirectoryByPathOrError $ stringToPath path) st
  modify (\s -> s { currentDirectory = newDirectory })

getContents :: StringPath -> FileSystem [SystemEntry]
getContents path = do
  dir <- getDirectoryByPathOrError $ stringToPath path
  return $ directoryContents dir

makeDirectory :: StringPath -> FileSystem ()
makeDirectory stringPath = do
  let path = stringToPath stringPath
  void $ createFile path emptyDirectory False

makeFile :: StringPath -> BS.ByteString -> FileSystem ()
makeFile stringPath text = do
  let path = stringToPath stringPath
  modificationTime <- liftIO $ systemToUTCTime <$> getSystemTime
  let file = (emptyDocument modificationTime) { documentContent = text }
  void $ createFile path file False

removeFile :: StringPath -> FileSystem ()
removeFile = FilesystemCore.removeFile . stringToPath

copyFile :: StringPath -> StringPath -> FileSystem ()
copyFile path targetPath = do
  file <- getFileByPath $ stringToPath path
  case file of
    Just f -> FilesystemCore.copyFile f (stringToPath targetPath)
    Nothing -> throwM NoSuchFile

appendToFile :: StringPath -> BS.ByteString -> FileSystem ()
appendToFile path text = do
  file <- getDocumentByPathOrError $ stringToPath path
  modificationTime <- liftIO $ systemToUTCTime <$> getSystemTime
  let newFile = file {
    documentUpdateTime = modificationTime,
    documentContent = documentContent file <> text
  }
  void $ createFile (stringToPath path) newFile True

readFileContents :: StringPath -> FileSystem BS.ByteString
readFileContents path = do
  file <- getDocumentByPathOrError $ stringToPath path
  return $ documentContent file

getFileInfo :: StringPath -> FileSystem String
getFileInfo path = do
  file <- getFileByPath $ stringToPath path
  case file of
    Just dir@Directory{} -> do
      let filepath = pathToString $ filePath dir
      let parentPath = show $ pathToString $ fileParent dir
      let permissions = filePermissions dir
      let fileCount = Prelude.length $ directoryContents dir
      let size = getFileSize dir
      return $ constructDirectoryInfo filepath parentPath permissions fileCount size
    Just doc@Document{} -> do
      let filepath = pathToString $ filePath doc
      let parentPath = show $ pathToString $ fileParent doc
      let permissions = filePermissions doc
      let extension = extensionFromPath $ filePath doc
      let creationTime = documentCreationTime doc
      let modificationTime = documentUpdateTime doc
      let size = getFileSize doc
      return $ constructDocumentInfo filepath parentPath permissions extension creationTime modificationTime size
    Nothing -> throwM NoSuchFile

constructDirectoryInfo :: StringPath -> StringPath -> Permissions -> Int -> Int64 -> String
constructDirectoryInfo path parentPath permissions fileCount size =
  "Path: " ++ path ++ "\n" ++
  "Parent: " ++ parentPath ++ "\n" ++
  "Permissions: " ++ show permissions ++ "\n" ++
  "Files: " ++ show fileCount ++ "\n" ++
  "Size: " ++ show size ++ "B"

constructDocumentInfo :: StringPath -> StringPath -> Permissions -> String -> UTCTime -> UTCTime -> Int64 -> String
constructDocumentInfo path parentPath permissions extension creationTime modificationTime size =
  "Path: " ++ path ++ "\n" ++
  "Parent: " ++ parentPath ++ "\n" ++
  "Permissions: " ++ show permissions ++ "\n" ++
  "SystemEntry type: " ++ extension ++ "\n" ++
  "Created at: " ++ show creationTime ++ "\n" ++
  "Updated at: " ++ show modificationTime ++ "\n" ++
  "Size: " ++ show size ++ "B"

getFileSize :: SystemEntry -> Int64
getFileSize dir@Directory{} = foldr ((+) . getFileSize) 0 (directoryContents dir)
getFileSize doc@Document{} = BS.length $ documentContent doc

findByName :: StringPath -> String -> FileSystem [String]
findByName rootPath name = do
  files <- FilesystemCore.findByName (stringToPath rootPath) name
  return $ Prelude.map (pathToString . filePath) files
