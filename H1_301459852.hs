--Lauren Yip CMPT383 
--301459852

-- 1. Fibonacci function
fib :: Int -> Int
fib 0 = 0 
fib 1 = 1
fib n = fib (n - 1) + fib (n - 2)

-- 2. List reverse function
listReverse :: [a] -> [a]
listReverse [] = []
listReverse (x:xs) = listReverse xs ++ [x]

-- 3. List addition function
listAdd :: [Int] -> [Int] -> [Int]
listAdd [] [] = []
listAdd [] (y:ys) = y : listAdd [] ys
listAdd (x:xs) [] = x : listAdd xs []
listAdd (x:xs) (y:ys) = (x + y) : listAdd xs ys

-- 4. Check if value is in list
inList :: Eq a => [a] -> a -> Bool
inList [] _ = False
inList (x:xs) v = (x == v) || inList xs v

-- 5. Tail-recursive sum function
sumTailRec :: Num a => [a] -> a
sumTailRec xs = helper xs 0
  where
    helper [] acc = acc
    helper (x:xs) acc = helper xs (acc + x)
