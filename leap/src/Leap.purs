module Leap where
import Prelude

isMultiple :: Int -> Int -> Boolean
isMultiple num1 num2 = (num1 `mod` num2) == 0

isLeapYear :: Int -> Boolean
isLeapYear year | (not <<< isMultiple year) 4 = false
  | (not <<< isMultiple year) 100 = true
  | (not <<< isMultiple year) 400 = false
  | otherwise = true

