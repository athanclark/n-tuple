# n-tuple


```haskell
{-# LANGUAGE DataKinds -#}

import Data.NTuple


foo :: NTuple 3 String
foo
  = incl _3 "three"
  . incl _2 "two"
  . incl _1 "one"
  $ empty


one :: String
one = proj _1 foo

two :: String
two = proj _2 foo
```
