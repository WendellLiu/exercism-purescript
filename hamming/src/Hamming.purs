module Hamming
  ( distance,
    tupleCharEqual
  ) where

import Prelude
import Data.String (length, toCharArray)
import Data.Maybe (Maybe(Just, Nothing))
import Data.Array (zip, foldl)
import Data.Tuple (Tuple(..))

isSameLong :: String -> String -> Boolean
isSameLong str1 = ((==) (length str1)) <<< length

zipTwoString :: String -> String -> Array (Tuple Char Char)
zipTwoString dna1 dna2 = zip (toCharArray dna1) (toCharArray dna2)

tupleCharEqual :: (Tuple Char Char) -> Boolean
tupleCharEqual (Tuple char1 char2) = char1 == char2


distance :: String -> String -> Maybe Int
distance dna1 dna2 = case isSameLong dna1 dna2 of
  false -> Nothing
  true -> Just (
    foldl (\pV cV -> pV + (if tupleCharEqual cV then 0 else 1) ) 0 (dna1 `zipTwoString` dna2)
  )