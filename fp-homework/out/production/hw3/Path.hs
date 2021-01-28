module Path where

import qualified Data.ByteString.Lazy.Char8 as BS
import Data.Digest.Pure.SHA
import Data.List (intercalate)
import Data.List.NonEmpty as NE
import Utils

type Path = NonEmpty String

type StringPath = String

toAbsolutePath :: Path -> Path -> Path -> Path
toAbsolutePath path rootPath currentPath =
  case path of
    "/" :| [] -> stringToPath "/"
    "/" :| cs -> _toAbsolutePath (NE.fromList cs) rootPath
    _ -> _toAbsolutePath path currentPath
  where
    _toAbsolutePath :: Path -> Path -> Path
    _toAbsolutePath path' rootPath' = foldl foldFunc rootPath' path'
      where
        foldFunc acc dir =
          case dir of
            "." -> acc
            ".." -> getParentPath acc
            s -> acc <:| s

nameByPath :: Path -> String
nameByPath = NE.last

stringToPath :: StringPath -> Path
stringToPath "/" = "/":|[]
stringToPath ('/':cs) = "/" :| NE.toList (splitOn '/' cs)
stringToPath s = splitOn '/' s

pathToString :: Path -> StringPath
pathToString ("/":|[]) = "/"
pathToString ("/":|cs) = "/" ++ (intercalate "/" cs)
pathToString path = intercalate "/" . NE.toList $ path

extensionFromPath :: Path -> String
extensionFromPath path =
  case splitOn '.' $ NE.last path of
    _ :| [] -> ""
    _ :| ext -> intercalate "." ext

getParentPath :: Path -> Path
getParentPath root@("/" :| []) = root
getParentPath list = NE.fromList $ NE.init list

isParentOf :: Path -> Path -> Bool
isParentOf parent path = (NE.toList parent) `NE.isPrefixOf` path

concatPath :: FilePath -> FilePath -> FilePath
concatPath "" path = path
concatPath parentPath "" = parentPath
concatPath parentPath path = parentPath ++ "/" ++ path

(</>) ::  FilePath -> FilePath -> FilePath
(</>) = concatPath

pathHash :: Path -> String
pathHash = show . sha1 . BS.pack . pathToString

emptyPath :: Path
emptyPath = "" :| []
