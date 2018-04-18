module Accumulate
  ( accumulate
  ) where

import Data.List.Types (List)
import Data.List (List(Nil), (:))


accumulate :: forall a b. (a -> b) -> List a -> List b
accumulate _ Nil = Nil
accumulate f (x : xs) = f x : accumulate f xs
