{-# LANGUAGE Rank2Types #-}

module Task67 where

import Control.Monad (guard, zipWithM)
import Data.List (isPrefixOf)
import Data.Maybe (fromMaybe)
import Lens.Micro (Lens', Traversal', filtered, lens, over, set, traversed, (^..))
import Lens.Micro.Extras (preview)
import System.Directory (doesDirectoryExist, doesFileExist, listDirectory)
import System.FilePath.Posix (splitDirectories, takeFileName, (</>))
import System.IO (FilePath)

data FS
    = Dir
    { _name     :: FilePath
    , _contents :: [FS]
    }
    | File
    { _name     :: FilePath
    }
    deriving (Show)

-- !traversed :: Traversable t => Traversal' (t a) a  // == (a -> f a) -> (t a -> f (t a))

-- Traversal' FS FilePath :: (FilePath -> f FilePath) -> (FS -> f FS)
dirName :: Traversal' FS FilePath
dirName _ file@(File _)  = pure file
dirName f (Dir nm conts) = Dir <$> f nm <*> pure conts

fileName :: Traversal' FS FilePath
fileName _ dir@(Dir _ _) = pure dir
fileName f (File n)      = File <$> f n

-- Traversal' FS [FS] :: ([FS] -> f [FS]) -> (FS -> f FS)
dirContents :: Traversal' FS [FS]
dirContents _ file@(File _) = pure file
dirContents f (Dir n x)     = Dir n <$> f x

---------------------
-- [1,2,3,4] ^.. each == [1,2,3,4]
subDirs :: FS -> [FS]
subDirs fs = fs ^.. (dirContents.traversed)

mbDirName :: FS -> Maybe FilePath
mbDirName = preview dirName

mbFileName :: FS -> Maybe FilePath
mbFileName = preview fileName

getFileName :: FS -> FilePath
getFileName fs = fromMaybe "" (mbFileName fs)

makeRoot :: FS -> FS
makeRoot = set dirName "/"

changeRoot :: String -> FS -> FS
changeRoot suffix = over dirName (++ suffix)

mbFirstDir :: FS -> Maybe FilePath
mbFirstDir = preview (dirContents.traversed.dirName)

getAllFileNames :: FS -> [FilePath]
getAllFileNames fs = fs ^.. dirContents.traversed.fileName

-----------------------------
--- 6
-----------------------------

getDirectory :: FilePath -> IO FS
getDirectory path = do
    isFile <- doesFileExist path
    if isFile
    then return File { _name = takeFileName path }
    else do
        isDir <- doesDirectoryExist path
        if isDir then do
            contentPaths <- listDirectory path
            contents <- traverse getDirectory $ (path </>) <$> contentPaths
            return Dir { _name = takeFileName path, _contents = contents }
        else fail $ "can't find path '" ++ path ++ "'"


-----------------------------
--- 7
-----------------------------

getName :: Lens' FS FilePath
getName = lens _name (\s a -> s {_name = a})

filterByName :: FilePath -> Traversal' FS FilePath -> Traversal' FS FS
filterByName name what = filtered (\fs -> preview what fs == Just name)

ls :: Traversal' FS FilePath
ls = dirContents.traversed.getName

cd :: FilePath -> Traversal' FS FS
cd dr = dirContents.traversed.filterByName dr dirName

file :: FilePath -> Traversal' FS FilePath
file fl = dirContents.traversed.filterByName fl fileName.fileName
