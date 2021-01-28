module Path
  ( Path
  , PathStr
  , toAbsolutePath
  , nameByPath
  , isParent
  , emptyPathFactory
  , getExtension
  , (</>)
  , toPath
  , toStr
  , simplify
  , getEntryName
  , showPath
  , showPathStr
  ) where

import Data.List (intercalate)
import Data.List.NonEmpty as NE (NonEmpty (..), fromList, init, isPrefixOf, last, toList)
import Data.List.Split (splitOn)
import Utils (nonAppendEl)

{-|
    The type containing a @NonEmpty@ list of folders/files name
-}
type Path = NonEmpty String

showPath :: Path -> String
showPath = toStr. simplifyCore

showPathStr :: String -> String
showPathStr = showPath . toPath

{-|
    A String representation of a @Path@ type
-}
type PathStr = String

{-|
    Converts from @PathStr@ to @Path@
-}
toPath :: PathStr -> Path
toPath "/"      = "/":|[]
toPath ('/':cs) = "/" :| splitOn "/" cs
toPath s        = fromList $ splitOn "/" s

{-|
    Converts from @Path@ to @PathStr@
-}
toStr :: Path -> PathStr
toStr ("/":|[]) = "/"
toStr ("/":|cs) = "/" ++ intercalate "/" cs
toStr path      = intercalate "/" . toList $ path

{-|
    Converts from @Path@ to @PathStr@

    Arguments:
    * 'path' to convert
    * 'rootPath' -- path to root of FS
    * 'curPath'  -- current directory of FS


    Returns 'path' converted to absolute representation
      depends on whether 'path' is absolute or not
-}
toAbsolutePath :: Path -> Path -> Path -> Path
toAbsolutePath path rootPath curPath =
  case path of
    "/" :| [] -> toPath "/"  -- if path is absolute and equals root
    "/" :| cs -> foldl foo rootPath (fromList cs)  -- elif path is absolute
    _         -> foldl foo curPath path  -- otherwise it's relative of curPath
  where
    foo acc dir =
      case dir of
        "."  -> acc
        ".." -> getParentPath acc
        s    -> acc `nonAppendEl` s

    getParentPath root@("/" :| []) = root
    getParentPath list             = NE.fromList $ NE.init list

{-|
    Checks whether (maybe)parent is a parent of the given path
-}
isParent :: Path -> Path -> Bool
isParent parent = isPrefixOf (toList parent)

{-|
    Concatenete parent and current paths, adding '/' if needed
-}
concatPaths :: FilePath -> FilePath -> FilePath
concatPaths "" path              = path
concatPaths dirPath ""           = dirPath
concatPaths dirPath ('/' : path) = dirPath ++ path
concatPaths "/" path             = "/" ++ path
concatPaths dirPath path         = dirPath ++ "/" ++ path

{-|
    Concatenete parent and current paths, adding '/' if needed
-}
(</>) ::  FilePath -> FilePath -> FilePath
(</>) = concatPaths

{-|
    Returns an empty @Path@
-}
emptyPathFactory :: Path
emptyPathFactory = "" :| []

{-|
    Returns an extension of the file by it's @Path@
-}
getExtension :: Path -> String
getExtension path =
  case Prelude.tail (splitOn "." $ NE.last path) of
    []  -> ""
    ext -> intercalate "." ext

simplifyCore :: Path -> Path
simplifyCore path = fromList $ reverse $ simp (toList path) [] where
  simp :: [String] -> [String] -> [String]
  simp [] acc                  = acc
  simp ("." : rest) acc        = simp rest acc
  simp (".." : rest) (_ : acc) = simp rest acc
  simp (name : rest) acc       = simp rest (name : acc)

{-|
    Simplify a @String@ representation of @Path@
-}
simplify :: String -> String
simplify path = toStr $ simplifyCore $ toPath path

{-|
    Returns name of file/directory by it's path
-}
nameByPath :: Path -> String
nameByPath path = NE.last $ simplifyCore path

getEntryName :: String -> String
getEntryName path = nameByPath $ toPath path
