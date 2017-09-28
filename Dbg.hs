module Dbg
( dbg
, db
) where

import Debug.Trace (trace, traceShow)
import Logging (logging)

-- Debug uses trace to output the first parameter and return the second
dbg :: String -> r -> r
dbg s r
        | logging = trace ("dbg: " ++ show s) r
        | otherwise = r

-- Allow you to use infix notation to call dbg placing
-- the return value on the left hand side. Thus it is
-- trivial to comment out the dbg using "--`db` ...
-- [See](https://en.wikibooks.org/wiki/Haskell/Debugging)
db = flip dbg
