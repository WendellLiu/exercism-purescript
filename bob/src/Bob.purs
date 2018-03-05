module Bob
  ( hey
  ) where

import Prelude
import Data.String (toCharArray, singleton)
import Data.Array (snoc, last)
import Partial.Unsafe (unsafePartial)
import Data.Maybe

-- foo :: String -> Char
-- foo = (fromJust <<< last <<< toCharArray)


hey :: String -> String
-- hey str = singleton <<< unsafePartial <<< fromJust <<< last <<< toCharArray
hey str = case (last <<< toCharArray) str of
  Nothing -> "nooooo"
  Just s -> singleton s

-- hey str  = singleton (unsafePartial (fromJust (last (toCharArray str))))
