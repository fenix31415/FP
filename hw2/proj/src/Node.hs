{-# LANGUAGE LambdaCase        #-}
{-# LANGUAGE OverloadedStrings #-}

module Node (Node(..), getOnlyDirs, emptyDirFactory, emptyDocFactory, findInFolder, getNodeName) where

import Data.ByteString.Lazy (ByteString)
import Data.List (find)
import Data.Time (UTCTime)
import Path (Path, emptyPathFactory, nameByPath, toPath, toStr)
import System.Directory (Permissions (..), emptyPermissions)

{-|
    A node of the FS. Might be either 'Dir' or 'Doc'
-}
data Node
  = Directory
      { nodePath        :: Path        -- ^ a path to the folder
      , nodeParent      :: Path        -- ^ a path to the parent folder
      , nodePermissions :: Permissions -- ^ permissions of a folder
      , nodeChildrens   :: [Node]      -- ^ a 'list' of children @Node@'s
      }
  -- ^ This node is a folder
  | Document
      { nodePath        :: Path           -- ^ a path to the file
      , nodeParent      :: Path           -- ^ a path to the parent folder
      , nodePermissions :: Permissions    -- ^ permissions of a file
      , docContent      :: ByteString  -- ^ contents of a file
      , docCreationTime :: UTCTime        -- ^ time of creation
      , docUpdateTime   :: UTCTime        -- ^ time of last update
      }
  -- ^ This node is a file

{-|
    Compare @Node@'s using it's @Path@'s
-}
instance Eq Node where
  (==) a b = nodePath a == nodePath b

{-|
    A @String@ representation is just path to the node
-}
instance Show Node where
  show = toStr . nodePath

{-|
    Compare @Node@'s using it's @Path@'s
-}
instance Ord Node where
  (<=) a b = toStr (nodePath a) <= toStr (nodePath b)

{-|
    Creates an empty @Directory@
-}
emptyDirFactory :: Node
emptyDirFactory = Directory
  { nodePath = emptyPathFactory
  , nodeParent = toPath "/"
  , nodePermissions = defaultPermissionsFactory
  , nodeChildrens = []
  }

{-|
    Creates an empty @Document@
-}
emptyDocFactory :: UTCTime -> Node
emptyDocFactory creationTime = Document
    { nodePath = emptyPathFactory
    , nodePermissions = defaultPermissionsFactory
    , nodeParent = toPath "/"
    , docCreationTime = creationTime
    , docUpdateTime = creationTime
    , docContent = ""
    }

{-|
    Creates a default @Permissions@ instance (666)
-}
defaultPermissionsFactory :: Permissions
defaultPermissionsFactory = emptyPermissions {
  readable = True,
  writable = True,
  executable = False,
  searchable = True
}

{-|
    Returns a 'list' storing only @Directory@'s
-}
getOnlyDirs :: [Node] -> [Node]
getOnlyDirs = filter (\case Directory{} -> True; _ -> False)

{-|
    Returns a file/directory's name by it's @Path@
-}
getNodeName :: Node -> String
getNodeName = nameByPath . nodePath

{-|
    Try to find a @Node@ amoung children of a given folder
-}
findInFolder :: Node -> String -> Maybe Node
findInFolder folder name = find ((name ==) . getNodeName) (nodeChildrens folder)
