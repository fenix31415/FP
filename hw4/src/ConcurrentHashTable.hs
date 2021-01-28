{-# LANGUAGE TupleSections #-}
module ConcurrentHashTable
  ( newCHT
  , getCHT
  , putCHT
  , sizeCHT
  , ConcurrentHashTable
  ) where

import Control.Concurrent.STM (STM, TVar, atomically, newTVar, readTVar, readTVarIO, writeTVar)
import Control.Exception.Base (mask_)
import Control.Monad (forM, forM_)
import Data.Hashable (Hashable, hash)
import Data.Vector ((!))
import qualified Data.Vector as V

type Cell k v = TVar [(k, v)]

data ConcurrentHashTable k v = ConcurrentHashTable (TVar (V.Vector (Cell k v))) (TVar Int)

-- | Return empty structure
newCHT :: IO (ConcurrentHashTable k v)
newCHT = mask_ $ atomically $ do
  newVector <- do
    initData <- V.replicateM 10 (newTVar [])
    newTVar initData
  newSize <- newTVar 0
  return $ ConcurrentHashTable newVector newSize

-- | Return 'cell' by the given key
getCell :: (Hashable k) => V.Vector (Cell k v) -> k -> STM (Cell k v)
getCell vector key = pure $ vector ! (hash key `mod` V.length vector)

-- | Return `Just value` if 'key' is present and 'Nothing' otherwise
getCHT :: (Hashable k, Eq k) => k -> ConcurrentHashTable k v -> IO (Maybe v)
getCHT key (ConcurrentHashTable curTData _) = mask_ $ atomically $ do
  curData <- readTVar curTData
  cellT <- getCell curData key
  cell <- readTVar cellT
  return $ lookup key cell

-- | Insert pair `(key, value)` without checking
insert :: (Hashable k) => V.Vector (Cell k v) -> (k, v) -> STM ()
insert vector (k, v) = do
  cell <- getCell vector k
  curList <- readTVar cell
  let newList = (k, v) : curList
  writeTVar cell newList

-- | Insert the given pair `(key, value)` to the given ConcurrentHashTable and resize if needed
putCHT :: (Hashable k, Eq k) => k -> v -> ConcurrentHashTable k v -> IO ()
putCHT key value (ConcurrentHashTable curTVector curTSize) = mask_ $ atomically $ do
  curVector <- readTVar curTVector
  cellT <- getCell curVector key
  cell <- readTVar cellT
  curSize <- readTVar curTSize
  case lookup key cell of
    Just _  -> writeTVar cellT $ map (\(k', v') -> (k',) $ if k' == key then value else v') cell
    Nothing -> do
      writeTVar curTSize (curSize + 1)
      if (fromIntegral curSize / fromIntegral (V.length curVector) :: Double) >= 0.5  -- resizing
      then do
        oldData <- forM (V.toList curVector) readTVar
        let newData = (key, value) : concat oldData
        newVector <- V.replicateM (2 * V.length curVector) (newTVar [])
        forM_ newData $ insert newVector
        writeTVar curTVector newVector
      else writeTVar cellT $ (key, value) : cell

-- | Return size of hashtable
sizeCHT :: ConcurrentHashTable k v -> IO Int
sizeCHT (ConcurrentHashTable _ size) = mask_ $ readTVarIO size
