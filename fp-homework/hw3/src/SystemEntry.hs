{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}

module SystemEntry where

import Data.List
import qualified Data.ByteString.Lazy as BS
import Data.Time (UTCTime)
import Path
import System.Directory

data SystemEntry
  = Directory
      { filePath :: Path
      , filePermissions :: Permissions
      , fileParent :: Path
      , directoryContents :: [SystemEntry]
      }
  | Document
      { filePath :: Path
      , filePermissions :: Permissions
      , fileParent :: Path
      , documentCreationTime :: UTCTime
      , documentUpdateTime :: UTCTime
      , documentContent :: BS.ByteString
      }

instance Eq SystemEntry where
  (==) a b = filePath a == filePath b && fileParent a == fileParent b

instance Show SystemEntry where
  show = pathToString . filePath
  
instance Ord SystemEntry where
  (<=) a b = (pathToString $ filePath a) <= (pathToString $ filePath b)

filterDirectories :: [SystemEntry] -> [SystemEntry]
filterDirectories = Prelude.filter (\case Directory{} -> True; _ -> False)

fileName :: SystemEntry -> String
fileName = nameByPath . filePath

findInFolder :: SystemEntry -> String -> Maybe SystemEntry
findInFolder folder name = find ((name ==) . fileName) (directoryContents folder)

isParentOf :: SystemEntry -> SystemEntry -> Bool
isParentOf parent file = (filePath parent) `Path.isParentOf` (filePath file)

emptyDirectory :: SystemEntry
emptyDirectory = Directory 
  { filePath = emptyPath
  , filePermissions = defaultPermissions
  , directoryContents = []
  , fileParent = stringToPath "/"
  }

emptyDocument :: UTCTime -> SystemEntry
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
