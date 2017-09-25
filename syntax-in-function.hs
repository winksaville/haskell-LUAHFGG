import Debug.Trace (trace, traceIO)

addVectors :: (Num a) => (a, a) -> (a, a) -> (a, a)
addVectors (x1, y1) (x2, y2) = (x1 + x2, y1 + y2)

head' :: [a] -> a
head' [] = error "empty list!"
head' (x:_) = x

tell :: (Show a) => [a] -> String
tell [] = error "Empty lst"
tell (x:[]) = "One item list: " ++ show x
tell (x:y:[]) = "Two item list: " ++ show x ++ " and " ++ show y
tell (x:xs) = "Multi item list: " ++ show (x:xs)

length' :: (Num b) => [a] -> b
length' ([]) = 0
length' (_:xs) = (1 + length' (xs))

-- How do I change sum' to print out the running total?
-- We need have the trace follow the recusive call so we
-- probably need a "do" statement
sum' :: (Show a, Num a) => [a] -> a
sum' ([]) = trace "empty" 0
sum' (x:xs) = trace ("current val: " ++ show x) (x + sum' xs)

-- Works although this isn't tail recursion so it will blow up
sum'2 :: (Show a, Num a) => [a] -> a
sum'2 ([]) = trace "empty" 0
sum'2 (x:xs) = do
    let total = x + sum'2 xs
    trace ("current val:" ++ show x ++ " total=" ++ show total) total

-- Test NOT tail recursion fails sum'3 [1..10000000]
sum'3 :: (Show a, Num a) => [a] -> a
sum'3 ([]) = trace "empty" 0
sum'3 (x:xs) = (sum'3 xs) + x

-- I thought this would be tail recursion but it also fails sum'4 [1..10000000]
sum'4 :: (Show a, Num a) => [a] -> a
sum'4 ([]) = trace "empty" 0
sum'4 (x:xs) = x + (sum'4 xs)

-- Define a tail recursion function to calcuate the a sum's of list elements.
-- because it take 2 parameters the second being the current total.
-- I currently feel using a loop and tempory for total is clearer and
-- AFAICT it would still be a pure function.
sumLst :: Integer -> [Integer] -> Integer
sumLst total [] = total
sumLst total (x:xs) = sumLst (x + total) xs

-- Logging version of sumLst, not sure how to make logging conditional
sumLstLog :: Integer -> [Integer] -> Integer
sumLstLog total [] = trace ("empty total=" ++ show total) total
sumLstLog total (x:xs) =
    let
        t = total + x
    in
        trace ("nxt val=" ++ show x ++ " total=" ++ show t)
        sumLstLog t xs

-- Use sumLst to calculate the sum
sum'5 :: [Integer] -> Integer
sum'5 lst = sumLst 0 lst

-- Tail recursion with "inner" function definition of sumLst called sumLoop
sum'6 :: [Integer] -> Integer
sum'6 lst = sumLoop lst 0 where
    sumLoop ([]) total = total
    sumLoop (x:xs) total = sumLoop xs (x + total)
