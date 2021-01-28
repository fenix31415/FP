{-# LANGUAGE OverloadedStrings #-}

module FSUtilsSpec where

import Control.Applicative ((<|>))
import Data.List (isPrefixOf, sort)
import Data.List.NonEmpty as NE (fromList, init, nonEmpty, tail)
import Data.Maybe (fromJust)
import FSUtils (FSState (fsstateCurDir, fsstateRoot), copyFile, createFile, createFileByName,
                findNode, getDocs, mbFindNode, removeFile, toAbsoluteFSPath)
import Node (Node (docContent, nodeChildrens, nodeParent, nodePath))
import Path (toPath, toStr)
import Test.Hspec (Spec, describe, it, shouldBe)
import Test.QuickCheck (Testable (property), forAll)
import TestUtils (cmpPathsStr, fenFile, fenixFSFactory, genFSAns, genFSDoc, genFSFiles, genFSFiles',
                  genFSPath, genFSPathcd, genFScdAns, myevalErrorFen, myevalShouldBe, newFile)
import Utils (nonAppendEl)

spec :: Spec
spec = do
  describe "Paths, paths, paths.." $ do
    it "many ppooints!" $ do
      let expected = toPath "/"
      let actual = toAbsoluteFSPath (toPath "../1/2/.././..")
      myevalShouldBe fenixFSFactory actual expected
    it "fs=random, abs, PPointness" $ property $ forAll genFSPath   $ \(fs, (pp, p))       -> cmpPathsStr (myevalShouldBe fs) ("/" ++ p) ("/" ++ pp)
    it "fs=random, rel, PPointness" $ property $ forAll genFSPathcd $ \((fs, cd), (pp, p)) -> cmpPathsStr (myevalShouldBe fs) p (cd ++ "/" ++ pp)
  describe "finding" $ do
    let check fs anses corrupt = do
          mapM_ (\s -> do
            let expected = Nothing
            let actual = mbFindNode (toPath $ s ++ corrupt)
            myevalShouldBe fs actual expected) anses
    let corrupt c = property $ forAll genFSAns $ \(fs, anses) -> check fs anses c
    it "cant find all1" $ corrupt "/./z/./"
    it "cant find all2" $ corrupt "z"
    it "can find all" $ property $
      forAll genFSAns $ \(fs, anses) -> do
        mapM_ (\s -> do
          let expected = toPath s
          let actual = nodePath . fromJust <$> mbFindNode (toPath s)
          myevalShouldBe fs actual expected) anses
    it "can find all rel" $ property $
      forAll genFScdAns $ \(fs, anses) -> do
        mapM_ (\s -> do
          let expected = Just s
          let toFind = fromJust $ nonEmpty $ NE.tail $ nodePath s
          print $ toStr toFind
          print $ toStr $ nodePath $ fsstateCurDir fs
          print $ nodeChildrens $ fsstateCurDir fs
          let actual = mbFindNode toFind
          myevalShouldBe fs actual expected) anses
  describe "searching" $ do
    it "search from root" $ property $ forAll genFSFiles $ \(fs, anses) -> do
      myevalShouldBe fs (sort <$> findNode (toPath "/") "1") (sort anses)  -- seems coverage good here
    it "search from folders" $ property $ forAll genFSFiles' $ \(fs, (target, anses)) -> do
      let foo = map (toStr . nodePath)
      let expected = filter (/= target) $ filter (isPrefixOf (target ++ "/")) (foo anses)
      let newact = foo <$> findNode (toPath target) "1"  -- seems here too
      myevalShouldBe fs (sort <$> newact) (sort expected)
    it "getDocs" $ property $
      forAll genFSDoc $ \(fs, "/" : ans) -> do
        let actual = map (toStr . nodePath) (getDocs $ fsstateRoot fs)
        sort actual `shouldBe` sort ans
  describe "creating" $ do
    it "a" $ property $ forAll genFSPath $ \(fs, (p, _)) -> do
      let pth = toPath $ "/" ++ p
      let pthFile = pth `nonAppendEl` "new.txt"
      let expected = "new content"
      let actual = do
            _ <- createFileByName pth "new.txt" newFile False
            docContent . fromJust <$> mbFindNode pthFile
      myevalShouldBe fs actual expected
    it "overriding" $ do
      let expected = Just "very new content"
      let actual = do
            let path = toPath "/file.txt"
            _ <- createFile path (newFile { nodePath = path, nodeParent = path, docContent = "very new content" }) True
            file <- mbFindNode path
            return $ docContent <$> file
      myevalShouldBe fenixFSFactory actual expected
    it "error" $ do
      let actual = do
            let path = toPath "/file.txt/new.txt"
            _ <- createFile path newFile False
            mbFindNode path
      myevalErrorFen actual
  describe "cp rm" $ do
    it "cp" $ do
      let expected = Just fenFile {
          nodePath = toPath "/file_copy.txt"
        , nodeParent = toPath "/"
        }
      let actual = do
            copyFile (fromJust expected) (toPath "/")
            mbFindNode $ toPath "/file_copy.txt"
      myevalShouldBe fenixFSFactory actual expected
    it "rm" $ property $ forAll genFSDoc $ \(fs, "/" : ans) -> do
      let expected = Nothing
      let item = toPath $ last ans
      let rmpath = fromList $ NE.init item
      let actual = do
            removeFile rmpath
            mbRemovedDir <- mbFindNode rmpath
            mbRemovedDoc <- mbFindNode item
            return $ mbRemovedDir <|> mbRemovedDoc
      myevalShouldBe fs actual expected
