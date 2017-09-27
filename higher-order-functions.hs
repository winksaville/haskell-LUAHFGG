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

multThree :: (Num a) => a -> a -> a -> a
multThree x y z = x * y * z

compareWith100 :: (Num a, Ord a) => a -> Ordering
compareWith100 x = compare 100 x

-- Infix curried function "/"
divideBy10 :: (Floating a) => a -> a
divideBy10 = (/ 10)

-- To partially apply "-" you must use "subtract"
subtract4 :: (Floating a) => a -> a
subtract4 = (subtract 4)

-- Apply a function twice, the first parameter is a
-- function which takes a parameter and returns a
-- value of the same type
applyTwice :: (a -> a) -> a -> a
applyTwice f x = f (f x)

zipWith' :: (a -> b -> c) -> [a] -> [b] -> [c]
zipWith' _ [] _ = []
zipWith' _ _ [] = []
zipWith' f (x:xs) (y:ys) = f x y : zipWith' f xs ys

flip' :: (a -> b -> c) -> b -> a -> c
flip' f x y = f y x

quicksort :: (Show a, Ord a) => [a] -> [a]
quicksort [] = []
quicksort (x:xs) =
        let small = quicksort (filter (<=x `db` ("small x:=" ++ show x)) xs)
            big = quicksort (filter (> x `db` ("big x:=" ++ show x)) xs)
        in small ++ [x] ++ big

largestDivisible :: (Integral a) => a
largestDivisible = head (filter p [100000,99999..])
        where p x = x `mod` 3829 == 0

chain :: (Integral a) => a -> [a]
chain 1 = [1]
chain n | even n = n:chain (n `div` 2)
        | odd n = n:chain (n*3 + 1)

numLongChains :: Int
numLongChains = length (filter isLong (map chain [1..100]))
        where isLong xs = length xs > 15

-- Use a lambda
numLongChains' :: Int
numLongChains' = length (filter (\xs -> length xs > 15) (map chain [1..100]))


-- left Folds
sum' :: (Num a) => [a] -> a
sum' xs = foldl (\acc x -> acc + x) 0 xs

elem' :: (Eq a) => a -> [a] -> Bool
elem' y ys = foldl (\acc x -> if x == y then True else acc) False ys

map'' :: (a ->b) -> [a] -> [b]
map'' f xs = foldl (\acc x -> acc ++ [f x]) [] xs

-- right folds using right fold and the cons operator ":" is
-- much cheaper (faster) than using "++" needed for the left fold
map' :: (a -> b) -> [a] -> [b]
map' f xs = foldr (\x acc -> f x : acc) [] xs


reverse' :: [a] -> [a]
reverse' lst = foldl (\acc x -> x : acc) [] lst

-- reverse as eta reduction
reverse'' :: [a] -> [a]
reverse'' = foldl (\acc x -> x : acc) []

-- Normal
fn x = negate (tan (cos (max 50 x)))
-- Functional composition
fn' x = negate . tan . cos . max 50 $ x
-- Function composition with eta reduction
fn'' = negate . tan . cos . max 50

-- Other examples
oddSquareSum :: Integer
oddSquareSum = sum (takeWhile (<100) (filter odd (map (^2) [1..])))

-- Using composition
oddSquareSum' :: Integer
oddSquareSum' = sum . takeWhile (<100) . filter odd . map (^2) $ [1..]

-- But possibly more obvious
oddSquareSum'' :: Integer
oddSquareSum'' =
        let oddSquares =  filter odd $ map (^2) [1..]
            belowLimit = takeWhile (<100) oddSquares
        in  sum belowLimit
