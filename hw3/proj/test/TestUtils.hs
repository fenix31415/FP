{-# LANGUAGE OverloadedStrings #-}

module TestUtils where

import Control.Monad.State (evalStateT, replicateM)
import Data.ByteString.Lazy (ByteString)
import Data.List (intercalate)
import Data.List.NonEmpty as NE (NonEmpty ((:|)))
import qualified Data.List.NonEmpty as NE (length)
import Data.Time (Day (..), UTCTime (..), secondsToDiffTime)
import FSUtils (FSState (..), FileSystem, toAbsoluteFSPath)
import Node (Node (docContent, nodeChildrens, nodeParent, nodePath), emptyDirFactory,
             emptyDocFactory)
import Path (Path, PathStr, simplify, toPath, toStr, (</>))
import Test.Hspec (Expectation, anyException, shouldBe, shouldThrow)
import Test.QuickCheck (Arbitrary (arbitrary), Gen, choose, elements, resize, sized)

--------------------------------------
-- Comparing staff
-------------------------------------

cmpPaths :: (FileSystem Path -> Path -> Expectation) -> FileSystem Path -> Path -> Expectation
cmpPaths shouldBeArg actual expected = do
  actual `shouldBeArg` expected

cmpPathsStr :: (FileSystem Path -> Path -> Expectation) -> String -> String -> Expectation
cmpPathsStr cmpArg act expe = do
  let expected = toPath expe
  let actual = toAbsoluteFSPath (toPath act)
  cmpArg actual expected

myevalFS :: FSState -> FileSystem a -> IO a
myevalFS st fs = evalStateT fs st

myevalShouldBe :: (Eq a, Show a) => FSState -> FileSystem a -> a -> Expectation
myevalShouldBe st actual expected = do
  q <- myevalFS st actual
  q `shouldBe` expected

myevalShouldBe' :: (Eq a, Show a) => FSState -> FileSystem a -> a -> Expectation
myevalShouldBe' st actual expected = do
  q <- myevalFS st actual
  print q
  q `shouldBe` expected

myevalErrorFen :: (Eq a, Show a) => FileSystem a -> Expectation
myevalErrorFen actual = myevalFS fenixFSFactory actual `shouldThrow` anyException

---------------------------------------
-- Paths generators
---------------------------------------
simpleAlph :: String
simpleAlph = "abcdefg1234_"

genNameChar :: Gen Char
genNameChar = elements simpleAlph

genNameByLen :: Int -> Gen [Char]
genNameByLen l = resize l $ sized $ \n ->
  replicateM n genNameChar

genName :: Gen [Char]
genName = do
  l <- choose(1,10)
  genNameByLen l

genPathByLen :: Int -> Gen [Char]
genPathByLen l = resize l $ sized $ \n ->
  intercalate "/" <$> replicateM n genName

genPath :: Gen [Char]
genPath = do
  l <- choose(1,20)
  genPathByLen l

--- Genering Path with points

insertAt :: a -> Int -> [a] -> [a]
insertAt newElement _ [] = [newElement]
insertAt newElement i (a:as)
  | i <= 0 = newElement:a:as
  | otherwise = a : insertAt newElement (i - 1) as

pointifyOnceCore :: String -> Int -> Path -> Path
pointifyOnceCore str n (r :| s) = r :| insertAt str n s

pointifyOnce :: Int -> Path -> Path
pointifyOnce = pointifyOnceCore "."

addPoints :: Path -> [Int] -> Path
addPoints = foldr pointifyOnce

arbitraryList :: Arbitrary a => Int -> Gen [a]
arbitraryList mx = resize mx $ sized $
    \n -> do
      k <- choose (0, n)
      sequence [ arbitrary | _ <- [1..k] ]

pointify :: String -> Gen String
pointify path = toStr <$> (addPoints pathPath <$> arbitraryList n) where
  pathPath = toPath path
  n = NE.length pathPath

genPontifyPath :: Gen (String, String)
genPontifyPath = do
  path <- genPath
  pointedPath <- pointify path
  return (path, pointedPath)

--- Genering paths with pointpoints

ppontifyOnce :: Int -> Path -> Path
ppontifyOnce = pointifyOnceCore ".."

addPPoints :: Path -> [Int] -> Path
addPPoints = foldr ppontifyOnce

ppointify :: Int -> String -> Gen String
ppointify n path = toStr . addPPoints (toPath path) <$> (map ((*3) . (+ 1)) <$> (filter (>0) <$> arbitraryList n))

genPPontifyPath :: Gen (String, String)
genPPontifyPath = do
  path <- genPath
  let n = length (toPath path) `div` 4
  ppointedPath <- ppointify n path
  pointedPath <- pointify ppointedPath
  return (simplify pointedPath, pointedPath)

genFSPath :: Gen (FSState, (String, String))
genFSPath = (,) <$> genSimpleFS <*> genPPontifyPath

genFSPathcd :: Gen ((FSState, String), (String, String))
genFSPathcd = (,) <$> genFScd <*> genPPontifyPath

---------------------------------------
-- FS Factory
---------------------------------------
emptyRootFactory :: Node
emptyRootFactory = createEmptyRoot "/"

fenDir :: Node
fenDir = createDir "/" fenixCD []

fenRootFactory :: Node
fenRootFactory = emptyDirFactory
  { nodePath = toPath "/"
  , nodeChildrens = [fenDir, fenFile]
  }

fenFile :: Node
fenFile = (emptyDocFactory emptyTime)
  { nodePath = toPath "/file.txt"
  , nodeParent = toPath "/"
  , docContent = "phoenix"
  }

emptyFSFactory :: FSState
emptyFSFactory = createFS emptyRootFactory emptyRootFactory ""

fenixPath :: PathStr
fenixPath = "/home/fenix"

fenixCD :: PathStr
fenixCD = "31415"

fenixFSFactory :: FSState
fenixFSFactory = createFS fenRootFactory fenDir fenixPath

createEmptyRoot :: String -> Node
createEmptyRoot path = emptyDirFactory
  { nodePath = toPath path
  }

createFS :: Node -> Node -> String -> FSState
createFS rootNode cdNode s = FSState
  { fsstateRoot = rootNode
  , fsstateCurDir = cdNode
  , fsstateAbsRoot = s
  }

---------------------------------------
--- FS Gen
---------------------------------------
createDir :: String -> String -> [Node] -> Node
createDir path name chs = emptyDirFactory
  { nodePath = toPath $ path </> name
  , nodeParent = toPath path
  , nodeChildrens = chs
  }

genDirsByLen' :: Int -> String -> Int -> Gen [Node]
genDirsByLen' l path depth = replicateM l (genDir path (depth - 1))

genDirsByLen :: Int -> String -> Int -> Gen [Node]
genDirsByLen l path depth = do
  chsNames <- genDistinctList [] l
  mapM (genDirbyName path (depth - 1)) chsNames

genDirsByLenAns :: Int -> String -> Int -> Gen ([Node], [String])
genDirsByLenAns l path depth = do
  chsNames <- genDistinctList [] l
  q <- unzip <$> mapM (genDirbyNameAns path (depth - 1)) chsNames
  return (fst q, concat $ snd q)

genDistinctList :: [String] -> Int -> Gen [String]
genDistinctList l 0 = pure l
genDistinctList l n = do
  name <- genName
  if name `elem` l
    then genDistinctList l n
    else genDistinctList (name : l) (n - 1)

genDirbyName :: String -> Int -> String -> Gen Node
genDirbyName path depth name = do
  l <- choose(0,depth)
  chs <- genDirsByLen l path (depth-1)
  return $ createDir path name chs

genDirbyNameAns :: String -> Int -> String -> Gen (Node, [String])
genDirbyNameAns path depth name = do
  l <- choose(0,depth)
  let q = path </> name
  (chs, anses) <- genDirsByLenAns l q depth
  return (createDir path name chs, q : anses)

genDir :: String -> Int -> Gen Node
genDir path depth = do
  name <- genName
  genDirbyName path depth name

genDirAns :: String -> Int -> Gen (Node, [String])
genDirAns path depth = do
  name <- genName
  genDirbyNameAns path depth name

genFSAns :: Gen (FSState, [String])
genFSAns = do
  (chs, anses) <- genDirsByLenAns 10 "/" 5
  let rootNode = createDir "" "/" chs
  return (createFS rootNode rootNode "/smth", "/" : anses)

createDoc :: String -> String -> ByteString -> Node
createDoc path name text = (emptyDocFactory emptyTime)
  { nodePath = toPath $ path </> name
  , nodeParent = toPath path
  , docContent = text
  }

genDocbyName :: String -> String -> Gen Node
genDocbyName path name = do
  return $ createDoc path name "123"

genDoc :: String -> Gen Node
genDoc path = do
  name <- genName
  genDocbyName path name

genDirbyNameDoc :: String -> Int -> String -> Gen (Node, [String])
genDirbyNameDoc path depth name = do
  l <- choose(0,depth)
  let q = path </> name
  docName' <- genName
  let docName = docName' ++ ".txt"
  doc <- genDocbyName q docName
  (chs, anses) <- genDirsByLenDoc l q depth
  return (createDir path name (doc : chs), q </> docName : anses)

genDirsByLenDoc :: Int -> String -> Int -> Gen ([Node], [String])
genDirsByLenDoc l path depth = do
  chsNames <- genDistinctList [] l
  q <- unzip <$> mapM (genDirbyNameDoc path (depth - 1)) chsNames
  return (fst q, concat $ snd q)

genFSDoc :: Gen (FSState, [String])
genFSDoc = do
  (chs, anses) <- genDirsByLenDoc 10 "/" 5
  let rootNode = createDir "" "/" chs
  return (createFS rootNode rootNode "/smth", "/" : anses)

genSimpleFS :: Gen FSState
genSimpleFS = do
  chs <- genDirsByLen 10 "/" 5
  let rootNode = createDir "" "/" chs
  return $ createFS rootNode rootNode "/smth"

--- Saving cd info

go :: Int -> Node -> Node
go 0 node = node
go n node =
  case nodeChildrens node of
    []          -> node
    (nnode : _) -> go (n-1) nnode

genFScdCore :: Gen (FSState, Node)
genFScdCore = do
  chs <- genDirsByLen 10 "/" 5
  let rootNode = createDir "" "/" chs
  let cd = go 3 rootNode
  return (createFS rootNode cd "/smth", cd)

genFScd :: Gen (FSState, String)
genFScd = do
  (fs, cd) <- genFScdCore
  return (fs, toStr $ nodePath cd)

genFScdAns :: Gen (FSState, [Node])
genFScdAns = do
  (fs, cd) <- genFScdCore
  return (fs, nodeChildrens cd)

--- Saving file info

genDirbyNameFiles :: String -> Int -> String -> Gen (Node, [Node])
genDirbyNameFiles path depth name = do
  l <- choose(0,depth)
  let q = path </> name
  (chs, anses) <- genDirsByLenFiles l q depth
  let ansdir = createDir path name chs
  let ans = if name == "1"
              then ansdir : anses
              else anses
  return (ansdir, ans)

genDirsByLenFiles :: Int -> String -> Int -> Gen ([Node], [Node])
genDirsByLenFiles l path depth = do
  chsNames <- genDistinctList [] l
  q <- unzip <$> mapM (genDirbyNameFiles path (depth - 1)) chsNames
  return (fst q, concat $ snd q)

genFSFiles :: Gen (FSState, [Node])
genFSFiles = do
  (chs, anses) <- genDirsByLenFiles 10 "/" 5
  let rootNode = createDir "" "/" chs
  return (createFS rootNode rootNode "/smth", anses)

genFSFiles' :: Gen (FSState, (String, [Node]))
genFSFiles' = do
  (chs, anses) <- genDirsByLenFiles 10 "/" 5
  let rootNode = createDir "" "/" chs
  let targetFolder = go 2 rootNode
  return (createFS rootNode rootNode "/smth", (toStr $ nodePath targetFolder, anses))


------------------------------------------

emptyTime :: UTCTime
emptyTime = UTCTime
  { utctDay = ModifiedJulianDay 0
  , utctDayTime = secondsToDiffTime 0
  }

newFile :: Node
newFile = (emptyDocFactory emptyTime)
  { docContent = "new content"
  }
