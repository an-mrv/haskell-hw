{-1-}
fizzbuzz :: [String]
fizzbuzz = [if x `mod` 15 == 0 then "fizzbuzz" else if x `mod` 3 == 0 then "fizz" else if x `mod` 5 == 0 then "buzz" else show x | x <- [1..]]

{-2-}
dotsInCircle :: (Double, Double) -> Double -> [(Double, Double)] -> [(Double, Double)]
dotsInCircle (x, y) r arr = [(a, b) | (a, b) <- arr, ((a - x)^2 + (b - y)^2) <= r^2]

{-3-}
setAnd :: [Int] -> [Int] -> [Int]
setAnd lst1 lst2 = [x | x <- lst1, y <- lst2, x == y]

{-1-}
summ :: Int -> Int
summ n = if n `div` 10 == 0 then n else (n `mod` 10) + summ (n `div` 10)

{-2-}
volume :: Int -> Int
volume n = if n `div` 10 == 0 then 1 else 1 + volume (n `div` 10)

{-3-}
degree :: Int -> Bool
degree n = if n `mod` 2 == 1 && n /= 1 then False else if n >= 2 then degree (n `div` 2) else if n == 1 then True else False

{-6 Collatz Conjecture-}
count :: Int -> Int
count n = helper 0 n
helper acc 1 = acc
helper acc n = if n `mod` 2 == 0 then helper (acc + 1) (n `div` 2) else helper (acc + 1) (3 * n + 1)

maximumm :: Int -> Int
maximumm n = helper' 0 n
helper' acc 1 = acc
helper' acc n = if n `mod` 2 == 0 then if n > acc then helper' n (n `div` 2) else helper' acc (n `div` 2) else if n > acc then helper' n (3 * n + 1) else helper' acc (3 * n + 1)

alll :: Int -> (Int, Int)
alll n = helper'' 0 1 n 
helper'' acc1 acc2 1 = (acc1, acc2)
helper'' acc1 acc2 n = if n `mod` 2 == 0 && n > acc2 then helper'' (acc1 + 1) n (n `div` 2) else if n `mod` 2 == 0 && n <= acc2 then helper'' (acc1 + 1) acc2 (n `div` 2) else if n > acc2 then helper'' (acc1 + 1) n (3 * n + 1) else helper'' (acc1 + 1) acc2 (3 * n + 1)

{-7-}
logg :: Int -> Int
logg n = helpp 1 n
helpp acc n = if acc >= n then acc else helpp (acc * 2) n

{-8-}
mysort :: [Int] -> [Int]
mysort [] = []
mysort (x:xs) = 
    let small = mysort [n | n <- xs, n <= x]
        big = mysort [n | n <- xs, n > x]
    in small ++ [x] ++ big
