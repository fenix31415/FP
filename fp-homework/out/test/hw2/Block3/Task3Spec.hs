module Block3.Task3Spec where

import           Block3.Task3
import           Block3.Task1
import           Test.Hspec

spec :: Spec
spec = do
  let good = Just ((), "")
  it "PSP oks" $ do
    runParser parsePSP "" `shouldBe` good
    runParser parsePSP "()" `shouldBe` good
    runParser parsePSP "()()" `shouldBe` good
    runParser parsePSP "(()())" `shouldBe` good
    runParser parsePSP "()()()" `shouldBe` good
    runParser parsePSP "(()())()" `shouldBe` good

  it "PSP bads" $ do
    runParser parsePSP "." `shouldBe` Nothing
    runParser parsePSP "(" `shouldBe` Nothing
    runParser parsePSP ")" `shouldBe` Nothing
    runParser parsePSP "((())()" `shouldBe` Nothing
    runParser parsePSP "()()(()()))" `shouldBe` Nothing
    runParser parsePSP "()9()())()" `shouldBe` Nothing
    runParser parsePSP "()()()." `shouldBe` Nothing
    runParser parsePSP "()().()" `shouldBe` Nothing

  it "ints oks" $ do
    runParser parseInt "31415" `shouldBe` Just (31415 :: Int, "")
    runParser parseInt "10000000000" `shouldBe` Just (10000000000 :: Int, "")
    runParser parseInt "+10000000000" `shouldBe` Just (10000000000 :: Int, "")
    runParser parseInt "-10000000000" `shouldBe` Just (-10000000000 :: Int, "")
    runParser parseInt "0" `shouldBe` Just (0 :: Int, "")
    runParser parseInt "+0" `shouldBe` Just (0 :: Int, "")
    runParser parseInt "-0" `shouldBe` Just (0 :: Int, "")

  it "ints bads" $ do
    runParser parseInt "10q" `shouldBe` Nothing
    runParser parseInt "10." `shouldBe` Nothing
    runParser parseInt "-154O45" `shouldBe` Nothing
    runParser parseInt "_456" `shouldBe` Nothing
    runParser parseInt "1.9874" `shouldBe` Nothing
    runParser parseInt "/9874" `shouldBe` Nothing
