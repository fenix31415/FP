{-# OPTIONS_GHC -fno-warn-type-defaults #-}  -- too annoying, sorry
{-# LANGUAGE OverloadedStrings, ScopedTypeVariables #-}

module FilesystemCoreSpec where

import Control.Applicative ((<|>))
import Data.List (sort)
import Data.Maybe (fromJust)
import File
import FilesystemCore
import Test.Hspec
import TestUtils

spec :: Spec
spec = do
  describe "FileSystem.toAbsoluteFSPath" $ do
    it "constructs absolute from relative" $ do
      let expected = toPath "/dir1"
      let actual = toAbsoluteFSPath (toPath "dir1")
      actual `evalShouldBe` expected
    it "correctly handles ." $ do
      let expected = toPath "/dir1"
      let actual = toAbsoluteFSPath (toPath "/./dir1/./.")
      actual `evalShouldBe` expected
    it "correctly handles .." $ do
      let expected = toPath "/"
      let actual = toAbsoluteFSPath (toPath "/dir1/dir2/../..")
      actual `evalShouldBe` expected
    it "correctly handles .. on root" $ do
      let expected = toPath "/"
      let actual = toAbsoluteFSPath (toPath "/../../../../..")
      actual `evalShouldBe` expected
    it "canonicalize path if it's already absolute" $ do
      let expected = toPath "/dir1"
      let actual = toAbsoluteFSPath (toPath "/dir1/.././dir1")
      actual `evalShouldBe` expected
  describe "FileSystem.getFileByPath" $ do
    it "gets file by absolute path" $ do
      let expected = Just dir1
      let actual = getFileByPath (toPath "/dir1")
      actual `evalShouldBe` expected
    it "gets file by relative path" $ do
      let expected = Just dir1_dir2
      let actual = getFileByPath (toPath "dir1/dir2")
      actual `evalShouldBe` expected
    it "returns Nothing if file does not exist" $ do
      let expected = Nothing
      let actual = getFileByPath (toPath "/what")
      actual `evalShouldBe` expected
  describe "FileSystem.findByName" $ do
    it "finds file in given directory recursively" $ do
      let expected = [file1, dir1_file1]
      let actual = findByName (toPath "/") "file1.txt"
      actual `evalShouldBe` expected
    it "finds file only in given directory and its subdirectories" $ do
      let expected = [dir1_file1]
      let actual = findByName (toPath "/dir1") "file1.txt"
      actual `evalShouldBe` expected
  describe "FileSystem.createFileByName" $ do
    it "creates file relatively to given parent path" $ do
      let expected = Just newFile {
          filePath = toPath "/dir1/new.txt"
        , fileParent = toPath "/dir1"
        }
      let actual = do
            _ <- createFileByName (toPath "/dir1") "new.txt" newFile False
            getFileByPath (toPath "/dir1/new.txt")
      actual `evalShouldBe` expected
  describe "FileSystem.createFile" $ do
    it "creates file by given path" $ do
      let expected = Just newFile {
          filePath = toPath "/dir1/new.txt"
        , fileParent = toPath "/dir1"
        }
      let actual = do
            let path = toPath "/dir1/new.txt"
            _ <- createFile path newFile False
            getFileByPath (toPath "/dir1/new.txt")
      actual `evalShouldBe` expected
    it "creates file by given path with missing directories" $ do
      let expected = Just newFile {
          filePath = toPath "/dir2/new.txt"
        , fileParent = toPath "/dir2"
        }
      let actual = do
            let path = toPath "/dir2/new.txt"
            _ <- createFile path newFile False
            getFileByPath path
      actual `evalShouldBe` expected
    it "overrides file if needed" $ do
      let expected = Just "new content"
      let actual = do
            let path = toPath "/file1.txt"
            _ <- createFile path (newFile { filePath = path, fileParent = path }) True
            file <- getFileByPath path
            return $ documentContent <$> file
      actual `evalShouldBe` expected
    it "fails when there is a file existing in path" $ do
      let actual = do
            let path = toPath "/file1.txt/new.txt"
            _ <- createFile path newFile False
            getFileByPath path
      evalShouldThrow actual
  describe "FileSystem.getAllFilesInSubDirectories" $ do
      it "retrieves all files in given directory recursively" $ do
        let expected = [file1, dir1_file1]
        let actual = getAllFilesInSubDirectories root
        sort actual `shouldBe` sort expected
  describe "FileSystem.copyFile" $ do
      it "copies file to given destination" $ do
        let expected = Just dir1_file1 {
            filePath = toPath "/file2.txt"
          , fileParent = toPath "/"
          }
        let actual = do
              copyFile (fromJust expected) (toPath "/")
              getFileByPath $ toPath "/file2.txt"
        actual `evalShouldBe` expected
  describe "FileSystem.removeFile" $ do
      it "removes file" $ do
        let expected = Nothing
        let actual = do
              removeFile $ toPath "/dir1"
              mbRemovedDir <- getFileByPath $ toPath "/dir1"
              mbRemovedFile <- getFileByPath $ toPath "/dir1/file1.txt"
              return $ mbRemovedDir <|> mbRemovedFile
        actual `evalShouldBe` expected

