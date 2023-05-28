module Lab2 where

import Prelude

import Data.List (List(..), foldr, reverse, (:))
import Data.Maybe (Maybe(..))
import Data.Tuple (Tuple(..))
import Effect (Effect)
import Effect.Console (logShow)

findIndex :: forall a. (a -> Boolean) -> List a -> Maybe Int
findIndex p xs = go 0 xs
  where
    go _ Nil = Nothing -- If the list is empty, return Nothing
    go i (Cons x xs') = if p x then Just i else go (i + 1) xs'

  
findLastIndex :: forall a. (a -> Boolean) -> List a -> Maybe Int
findLastIndex fn xs = go 0 xs Nothing
  where
    go :: Int -> List a -> Maybe Int -> Maybe Int
    go _ Nil idx = idx
    go n (Cons a as) idx | fn a = go (n + 1) as (Just n)
                        | otherwise = go (n + 1) as idx

zip :: forall a b. List a -> List b -> List (Tuple a b)
zip Nil _ = Nil 
zip _ Nil = Nil 
zip (Cons x xs) (Cons y ys) = Cons (Tuple x y) (zip xs ys)

unzip :: forall a b. List (Tuple a b) -> Tuple (List a) (List b)
unzip Nil = Tuple Nil Nil
unzip (Cons (Tuple x y) xs) =
  let Tuple xs' ys' = unzip xs
  in Tuple (Cons x xs') (Cons y ys')

filter :: forall a. (a -> Boolean) -> List a -> List a
filter p = foldr (\x acc -> if p x then x : acc else acc) Nil

tailRecursionFilter :: forall a. (a -> Boolean) -> List a -> List a
tailRecursionFilter fn xs = reverse $ go xs Nil
  where
    go :: List a -> List a -> List a
    go Nil acc = acc
    go (Cons x xs) acc | fn x = go xs (Cons x acc)
                      | otherwise = go xs acc

take :: forall a. Int -> List a -> List a
take n _ | n <= 0 = Nil
take _ Nil = Nil 
take n (Cons x xs) = Cons x (take (n - 1) xs) 

tailRecursionTake :: forall a. Int -> List a -> List a
tailRecursionTake n xs = go n xs Nil
  where
    go 0 _ acc = reverse acc
    go _ Nil acc = reverse acc
    go i (Cons x xs) acc = go (i - 1) xs (Cons x acc)

test :: Effect Unit
test = do 
  let mylist = (Cons 1 (Cons 2(Cons 3 Nil)))
  let mylist2 = (Cons 5 (Cons 10(Cons 15 Nil)))

  logShow $ "findIndex"
  logShow $ findIndex(_==1) mylist -- output: Just 0
  logShow $ findIndex(_==10) mylist -- output: Nothing
  
  logShow $ "findLastIndex"
  logShow $ findLastIndex(_==1) mylist

  logShow $ "zip"
  let zipped = zip mylist mylist2
  logShow zipped

  logShow $ "unzip"
  logShow $ unzip zipped

  logShow $ "filter"
  let evenNumbers = filter (\x -> x `mod` 2 == 0) mylist2
  logShow $ "Even numbers from list [5, 10, 15]"
  logShow evenNumbers -- [10]
    
  logShow $ "tailRecursionFilter"
  logShow $ tailRecursionFilter(_<2) mylist --[1]
    
  logShow $ "take"
  logShow $ take(2) mylist2  

  logShow $ "tailRecursionTake"
  logShow $ tailRecursionTake(2) mylist2