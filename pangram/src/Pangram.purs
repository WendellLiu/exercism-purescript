module Pangram
  ( isPangram
  ) where

import Prelude
import Data.Either (Either(Right))
import Data.String (toLower)
import Data.String.Regex (Regex, regex, test)
import Data.String.Regex.Flags (noFlags)
import Partial.Unsafe (unsafePartial)

testRe :: Regex
testRe = unsafePartial case regex "(?=.*a)(?=.*b)(?=.*c)(?=.*d)(?=.*e)(?=.*f)(?=.*g)(?=.*h)(?=.*i)(?=.*j)(?=.*k)(?=.*l)(?=.*m)(?=.*n)(?=.*o)(?=.*p)(?=.*q)(?=.*r)(?=.*s)(?=.*t)(?=.*u)(?=.*v)(?=.*w)(?=.*x)(?=.*y)(?=.*z)." noFlags of
                Right r -> r


isPangram :: String -> Boolean
isPangram = (test testRe <<< toLower)