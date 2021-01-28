module FilesystemDumper
  ( dumpFilesystem
  ) where

import Control.Monad.State
import qualified Data.ByteString.Lazy.Char8 as BS
import Data.List.NonEmpty as NE
import File
import FilesystemCore (InfoFS(..))
import Path
import System.Directory

{-|
  Dump @FSState@ to real filesystem.
  For extra safety backup to .fm_old is made before replacement of files.
-}
dumpFilesystem :: InfoFS -> IO ()
dumpFilesystem fsState = do
  let rootPath = realRootPath fsState
  setCurrentDirectory rootPath
  let backupPath = rootPath </> ".fm_old"
  removePathForcibly backupPath
  copyDir rootPath backupPath
  removeAllExceptBackup rootPath
  dumpFile (rootDirectory fsState) rootPath
  return ()

copyDir ::  FilePath -> FilePath -> IO ()
copyDir src dest = do
  createDirectory dest
  contents <- Prelude.filter (`notElem` [".", ".."]) <$> getDirectoryContents src
  forM_ contents $ \name -> do
    let srcPath = src </> name
    let destPath = dest </> name
    unless (srcPath == dest) $ do
      isDirectory <- doesDirectoryExist srcPath
      if isDirectory
        then copyDir srcPath destPath
        else copyFile srcPath destPath

removeAllExceptBackup ::  FilePath -> IO ()
removeAllExceptBackup src = do
  content <- Prelude.filter (`notElem` [".", "..", ".fm_old"]) <$> getDirectoryContents src
  forM_ content $ \name -> removePathForcibly (src </> name)

dumpFile :: File -> FilePath -> IO ()
dumpFile root realFsRoot =
  case root of
    Directory{ filePath = path, directoryContents = contents } -> do
      print (toRealPath path realFsRoot)
      createDirectoryIfMissing False (toRealPath path realFsRoot)
      forM_ contents (`dumpFile` realFsRoot)
    Document{ filePath = path, documentContent = contents } -> do
      print (toRealPath path realFsRoot)
      writeFile (toRealPath path realFsRoot) (BS.unpack contents)

toRealPath :: Path -> FilePath -> FilePath
toRealPath path rootPath =
  case path of
    "/" :| [] -> rootPath
    "/" :| [x] -> rootPath </> x
    "/" :| x : xs -> rootPath </> (pathToString $ x :| xs)
    _ -> error "path expected to be absolute"
