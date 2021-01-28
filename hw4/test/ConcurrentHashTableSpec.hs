module ConcurrentHashTableSpec where

import ConcurrentHashTable (getCHT, newCHT, putCHT, sizeCHT)
import Control.Concurrent (threadDelay, throwTo)
import qualified Control.Concurrent.Thread as Th
import Control.Exception.Base (AsyncException (ThreadKilled), handle, throwTo)
import Control.Monad (forM, forM_, replicateM, replicateM_)
import Test.Hspec (Spec, describe, it, shouldBe)
import Test.QuickCheck (Testable (property), forAll)

handler :: AsyncException -> IO ()
handler e = putStrLn $ "async exception: " <> show e

spec :: Spec
spec = do
    it "size&puts" $ do
        let len = 100
        ht <- newCHT
        forM_ (take len $ map show [0..]) $ \i -> do
            size <- sizeCHT ht
            size `shouldBe` read i
            putCHT i (1 :: Int) ht
    it "puts&gets" $ do
        let len = 100
        ht <- newCHT
        --replicateM_ 10 (putCHT "1" 1 ht)
        forM_ [1..len] $ \i -> putCHT (show i) (i::Int) ht
        size <- sizeCHT ht
        size `shouldBe` len
        forM_ [1..len] $ \i -> do
            t <- getCHT (show i) ht
            t `shouldBe` Just i
    it "exceptions" $ do
        let len = 100
        ht <- newCHT
        (threadId, res') <- Th.forkIO $ do
            threadDelay 10
            forM_ [1..len] $ \i -> putCHT (i::Int) (i::Int) ht
        threadDelay 30
        throwTo threadId ThreadKilled
        res <- res'
        handle handler $ Th.result res
        curData <- forM [1..len] $ \i -> getCHT i ht
        forM_ (zip curData [1..len]) $ \v -> case v of
            (Just _, i) -> v `shouldBe` (Just i, i)
            _           -> pure ()
