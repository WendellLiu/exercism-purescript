module Hamming
  ( distance,
    isSameLong
  ) where

import Prelude
import Data.String (length)
import Data.Maybe (Maybe(Just, Nothing))

isSameLong :: String -> String -> Boolean
isSameLong str1 = ((==) (length str1)) <<< length

distance :: String -> String -> Maybe Int
distance dna1 dna2 = case isSameLong dna1 dna2 of
  false -> Nothing
  _ -> Just 1