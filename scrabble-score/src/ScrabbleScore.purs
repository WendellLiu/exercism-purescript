module ScrabbleScore
  ( scoreWord
  ) where

import Prelude
import Data.String (toCharArray, toUpper)
import Data.Foldable (sum, find)
import Data.Maybe (Maybe(Just, Nothing))

findInArray :: forall a. Eq a => Array a -> a -> Boolean
findInArray parentArray ele = case find (\n -> n == ele) parentArray of
  Just _ -> true
  Nothing -> false

score :: Char -> Int
score char
  | findInArray ['A', 'E', 'I', 'O', 'U', 'L', 'N', 'R', 'S', 'T'] char = 1
  | findInArray ['D', 'G'] char = 2
  | findInArray ['B', 'C', 'M', 'P'] char = 3
  | findInArray ['F', 'H', 'V', 'W', 'Y'] char = 4
  | findInArray ['K'] char = 5
  | findInArray ['J', 'X'] char = 8
  | findInArray ['Q', 'Z'] char = 10
  | otherwise = 0

  

scoreWord :: String -> Int
scoreWord = (sum <<< (map score) <<< toCharArray <<< toUpper)