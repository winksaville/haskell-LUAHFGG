import Debug.Trace (trace, traceShow)

-- Enable/Disable logging by dbg function
logging = True

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

maximum' :: (Show a, Ord a) => [a] -> a
maximum' [] = error "List is empty, no maximum!"
maximum' [x] = x `db` ("one element: " ++ show x)
maximum' (x:xs)
         | x > maxTail = x `db` ("x:xs x:=" ++ show x ++ " > maxTail:=" ++ show maxTail)
         | otherwise = maxTail `db` ("x:xs x:=" ++ show x ++ " otherwise maxTail:=" ++ show maxTail)
         where
                maxTail' = (maximum' xs) `db` ("x:xs x:=" ++ show x ++ " where maxTail = maximum' xs:=" ++ show xs)
                maxTail = maxTail' `db` ("x:xs^x:=" ++ show x ++ " where maxTail:=" ++ show maxTail')

replicate' :: (Show a, Show n, Num n, Ord n) => n -> a -> [a]
replicate' n x
        | n <= 0 = [] --`db` ("n:=" ++ show n ++ " <= 0 x:=" ++ show x)
        | otherwise = x:replicate' (n-1) x --`db` ("n:=" ++ show (n-1) ++ " x:=" ++ show x)
