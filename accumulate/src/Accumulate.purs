module Accumulate
  ( accumulate
  ) where

import Prelude
import Data.List.Types (List)
import Data.List (concat, null, head, tail, singleton)


accumulate :: forall a. (a -> a) -> List a -> List a
accumulate acc f = case null f of
  false -> concat (singleton acc (head f)) $ accumulate acc (tail f)
  true -> acc f
