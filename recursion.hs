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

-- Show a, Show n is needed for dbg :(
-- replicate' :: (Show a, Show n, Num n, Ord n) => n -> a -> [a]
replicate' :: (Num n, Ord n) => n -> a -> [a]
replicate' n x
        | n <= 0 = [] --`db` ("n:=" ++ show n ++ " <= 0 x:=" ++ show x)
        | otherwise = x:replicate' (n-1) x --`db` ("n:=" ++ show (n-1) ++ " x:=" ++ show x)

-- Zip using patterns
zip' :: (Show a, Show b) => [a] -> [b] -> [(a,b)]
zip' [] _ = [] `db` "first list is empty, return []"
zip' _ [] = [] `db` "second list is empty, return []"
zip' (a:as) (b:bs) = (a,b):zip' as bs `db` ("two lists a:=" ++ show a ++ " as:=" ++ show as ++ " b:=" ++ show b ++ " bs:=" ++ show bs)

-- Zip using guards instead of pattern's
zip'' :: (Show a, Show b) => [a] -> [b] -> [(a,b)]
zip'' l1 l2
 | length(l1) == 0 || length(l2) == 0 = [] `db` "A list is empty, return []"
 | otherwise = let (x:xs) = l1
                   (y:ys) = l2
               in  (x,y):zip'' xs ys `db` ("two lists x:=" ++ show x ++ " xs:=" ++ show xs ++ " y:=" ++ show y ++ " ys:=" ++ show ys)
