{-# LANGUAGE OverloadedStrings #-}

module TestUtils where

import Data.Time (UTCTime(..), Day(..), secondsToDiffTime)
import Control.Monad.State
import SystemEntry
import SystemEntrysystemCore
import Path
import Test.Hspec

toPath :: String -> Path
toPath = stringToPath

evalFS :: SystemEntrySystem a -> IO a
evalFS fs = evalStateT fs fsState

evalShouldBe :: (Eq a, Show a) => SystemEntrySystem a -> a -> Expectation
evalShouldBe actual expected = evalFS actual >>= (`shouldBe` expected)

evalShouldThrow :: (Eq a, Show a) => SystemEntrySystem a -> Expectation
evalShouldThrow actual = evalFS actual `shouldThrow` anyException

errorHandler :: (Eq a, Show a) => CommandExecutionError -> SystemEntrySystem (Maybe a)
errorHandler _ = return Nothing

emptyTime :: UTCTime
emptyTime = UTCTime
  { utctDay = ModifiedJulianDay 0
  , utctDayTime = secondsToDiffTime 0
  }

fsState :: FSInfo
fsState = FSInfo
  { rootDirectory = root
  , currentDirectory = root
  , realRootPath = "/home/newuserkk"
  }

root :: SystemEntry
root = emptyDirectory
  { SystemEntryPath = toPath "/"
  , directoryContents = [dir1, SystemEntry1]
  }

rootWithCVS :: SystemEntry
rootWithCVS = root
  { directoryContents = directoryContents root ++ [rootCVS]
  }

rootCVS :: SystemEntry
rootCVS = emptyDirectory
  { SystemEntryPath = toPath "/.cvs"
  , SystemEntryParent = toPath "/"
  }

dir1 :: SystemEntry
dir1 = emptyDirectory
  { SystemEntryPath = toPath "/dir1"
  , SystemEntryParent = toPath "/"
  , directoryContents = [dir1_dir2, dir1_SystemEntry1]
  }

dir1_dir2 :: SystemEntry
dir1_dir2 = emptyDirectory
  { SystemEntryPath = toPath "/dir1/dir2"
  , SystemEntryParent = stringToPath "/dir1"
  , directoryContents = []
  }

SystemEntry1 :: SystemEntry
SystemEntry1 = (emptyDocument emptyTime)
  { SystemEntryPath = toPath "/SystemEntry1.txt"
  , SystemEntryParent = toPath "/"
  , documentContent = "КОНТЕНТ"
  }

dir1_SystemEntry1 :: SystemEntry
dir1_SystemEntry1 = (emptyDocument emptyTime)
  { SystemEntryPath = toPath "/dir1/SystemEntry1.txt"
  , SystemEntryParent = toPath "/dir1"
  , documentContent = "ModifiedJulianDay"
  }

newSystemEntry :: SystemEntry
newSystemEntry = (emptyDocument emptyTime)
  { documentContent = "new content"
  }
