module Bob where

import Prelude

import Data.Array (last)
import Data.Maybe (Maybe(..))
import Data.String (toCharArray, toLower, toUpper, trim)

whoa :: String
whoa = "Whoa, chill out!"

whatever :: String
whatever = "Whatever."

sure :: String
sure = "Sure."

fine :: String
fine = "Fine. Be that way!"

getLastChar :: String -> Char
getLastChar str = case (last <<< toCharArray <<< trim) str of
  Nothing -> ' '
  Just s -> s

isAllUpper :: String -> Boolean
isAllUpper str = ((==) str <<< toUpper) str && ((/=) str <<< toLower) str

hey :: String -> String
hey str | ((==) "" <<< trim) str = fine 
  | isAllUpper str = whoa
  | ((==) '?' <<< getLastChar) str = sure
  | otherwise = whatever