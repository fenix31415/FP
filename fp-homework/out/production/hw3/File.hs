{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}

module File where

import Data.List
import qualified Data.ByteString.Lazy as BS
import Data.Time (UTCTime)
import Path
import System.Directory

data File
  = Directory
      { filePath :: Path
      , filePermissions :: Permissions
      , fileParent :: Path
      , directoryContents :: [File]
      }
  | Document
      { filePath :: Path
      , filePermissions :: Permissions
      , fileParent :: Path
      , documentCreationTime :: UTCTime
      , documentUpdateTime :: UTCTime
      , documentContent :: BS.ByteString
      }

instance Eq File where
  (==) a b = filePath a == filePath b && fileParent a == fileParent b

instance Show File where
  show = pathToString . filePath
  
instance Ord File where
  (<=) a b = (pathToString $ filePath a) <= (pathToString $ filePath b)

filterDirectories :: [File] -> [File]
filterDirectories = Prelude.filter (\case Directory{} -> True; _ -> False)

fileName :: File -> String
fileName = nameByPath . filePath

findInFolder :: File -> String -> Maybe File
findInFolder folder name = find ((name ==) . fileName) (directoryContents folder)

isParentOf :: File -> File -> Bool
isParentOf parent file = (filePath parent) `Path.isParentOf` (filePath file)

emptyDirectory :: File
emptyDirectory = Directory 
  { filePath = emptyPath
  , filePermissions = defaultPermissions
  , directoryContents = []
  , fileParent = stringToPath "/"
  }

emptyDocument :: UTCTime -> File
emptyDocument creationTime = Document
  { filePath = emptyPath
  , filePermissions = defaultPermissions
  , fileParent = stringToPath "/"
  , documentCreationTime = creationTime
  , documentUpdateTime = creationTime
  , documentContent = ""
  }

defaultPermissions :: Permissions
defaultPermissions = emptyPermissions {
  readable = True,
  writable = True,
  executable = False,
  searchable = True
}
