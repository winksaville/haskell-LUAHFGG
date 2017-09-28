module Dbg
(db
) where

import Data.Int (Int8)
import Debug.Trace (trace, traceShow)
import Logging (logging)

-- DeBug (db) uses trace to output the last parameter and return the first.
-- Best used as infix, `db`, which allows the return value, retv, to be on LHS.
db :: r -> Int -> String -> r
db retv level str
        | level > logging = trace ("db" ++ show level ++ ": " ++ show str) retv
        | otherwise = retv
