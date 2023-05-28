module Lab1
  ( length
  , null
  , singleton
  , snoc
  , test
  )
  where

import Data.Array as Array
import Data.Foldable (foldr)
import Effect (Effect)
import Effect.Console (log)
import Prelude

-- Task 1: Singleton function
singleton :: forall a. a -> Array a
singleton x = [x]
-- Task 1: Using function from library
singleton2 :: forall a. a -> Array a
singleton2 = Array.singleton

-- Task 2: Null function
null :: forall a. Array a -> Boolean
null arr = length arr == 0
-- Task 2: Using function from library
null2 :: forall a. Array a -> Boolean
null2 = Array.null

-- Task 3: Snoc function
snoc :: forall a. Array a -> a -> Array a
snoc arr x = Array.snoc arr x
-- Task 3: Using function from library
snoc2 :: forall a. Array a -> a -> Array a
snoc2 = Array.snoc

-- Task 4: Length function
length :: forall a. Array a -> Int
length arr = foldr (\_ acc -> acc + 1) 0 arr
-- Task 4: Using function from library
length2 :: forall a. Array a -> Int
length2 = Array.length

test :: Effect Unit
test = do 
  log $ "Singleton"
  log $ show (singleton "test1")
  log $ "Singleton func"
  log $ show (singleton2 "test1")
  log $ "Null empty"
  log $ show (null [])
  log $ "Null empty func"
  log $ show (null2 [])
  log $ "Null nonEmpty"
  log $ show (null ["test"])
  log $ "Null nonEmpty func"
  log $ show (null2 ["test1"])
  log $ "Snoc" 
  log $ show (snoc ["test"] "test1")
  log $ "Snoc func" 
  log $ show (snoc2 ["test2"] "test21")
  log $ "Length"
  log $ show (length ["test", "test2", "test3"])
  log $ "Length func"
  log $ show (length ["test21", "test22", "test23"])