module Raindrops
  ( raindrops
  ) where

import Prelude
import Data.String (joinWith)
import Data.Int (toStringAs, radix)
import Partial.Unsafe (unsafePartial)
import Data.Maybe (fromJust)

isMultiple :: Int -> Int -> Boolean
isMultiple num1 num2 = (num2 `mod` num1) == 0

threeCallback :: Int -> String
threeCallback n = case isMultiple 3 n of
  true -> "Pling"
  false -> ""

fiveCallback :: Int -> String
fiveCallback n = case isMultiple 5 n of
  true -> "Plang"
  false -> ""

sevenCallback :: Int -> String
sevenCallback n = case isMultiple 7 n of
  true -> "Plong"
  false -> ""

callbackArray :: Array (Int -> String)
callbackArray = [
  threeCallback,
  fiveCallback,
  sevenCallback
]

raindrops :: Int -> String
raindrops n = case (joinWith ""  $ (\f -> f n) <$> callbackArray) of
  "" -> toStringAs (unsafePartial (fromJust (radix 10))) n
  a -> a