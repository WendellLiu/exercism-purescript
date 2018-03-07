module Hamming
  ( distance,
    isSameLong
  ) where

import Prelude
import Data.String (length, toCharArray)
import Data.Maybe (Maybe(Just, Nothing))
import Data.Array (zip)
import Data.Tuple (Tuple(..))

isSameLong :: String -> String -> Boolean
isSameLong str1 = ((==) (length str1)) <<< length

zipTwoString :: String -> String -> Array (Tuple Char Char)
zipTwoString dna1 dna2 = zip (toCharArray dna1) (toCharArray dna2)


distance :: String -> String -> Maybe Int
distance dna1 dna2 = case isSameLong dna1 dna2 of
  false -> Nothing
  true -> Just 1