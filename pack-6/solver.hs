--task 2
data Suit = Hearts | Tiles | Clovers | Pikes
        deriving Eq
type Value = Int
data Card = JokerA | JokerB | Card Suit Value
        deriving Eq

f :: [Card] -> Bool
f s = if length (f' s []) == 54 then True else False
f' [] acc = acc
f' (x:xs) acc | (x `elem` acc) = f' xs acc
              | otherwise = f' xs (x : acc)
              
--task 3
powerset :: [a] -> [[a]]
powerset [] = [[]]
powerset (x:xs) = [x:y | y <- powerset xs] ++ powerset xs

--task 4 (IpAdress1)
check :: [Char] -> Bool
check s = if length (split s) == 4 && not (False `elem` value (split s)) then True else False
        where value = foldl (\acc x -> (x > 0 && x < 255) : acc) []

split :: [Char] -> [Int]
split s = toint (split' s [])
        where split' [] acc = [acc]
              split' (x:xs) acc | (x == '.') = acc : (split' xs [])
                                | otherwise = split' xs (acc ++ [x])

toint :: [[Char]] -> [Int]
toint a = foldl (\acc x -> acc ++ [toint' x]) [] a
toint' s = foldl (\acc x -> digit x + acc * 10) 0 s
        where digit x |(x == '0') = 0
                      |(x == '1') = 1
                      |(x == '2') = 2 
                      |(x == '3') = 3
                      |(x == '4') = 4
                      |(x == '5') = 5
                      |(x == '6') = 6
                      |(x == '7') = 7
                      |(x == '8') = 8
                      |otherwise = 9

--task 7 (matrix)
transposition :: [[Char]] -> [[Char]]  
transposition m = [[m !! a !! b | a <- [0..(length (m) - 1)]] | b <- [0..(length (m !! 0) - 1)]]

--task 8
checker :: Int -> Bool
checker a = checker' a 0
checker' a n | (a == 1) = True
             | (n == 10000) = False
             | otherwise = checker' (summ_of_squares a 0) (n + 1) 
        where summ_of_squares a acc | (a == 0) = acc
                                    | otherwise = summ_of_squares (a `div` 10) (acc + (a `mod` 10) ^ 2) 