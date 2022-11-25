{-# LANGUAGE DeriveFoldable #-}

import Data.Maybe (isJust)

--task 1
data Tree a = Empty
            | Leaf a
            | Node (Tree a) a (Tree a)
    deriving (Eq, Foldable)

data Cat = Cat deriving (Eq)

tree1 :: Tree (Maybe Cat)
tree1 = Node Empty Nothing (Node (Node (Leaf Nothing) Nothing (Node Empty Nothing (Leaf (Just Cat)))) Nothing (Node (Leaf Nothing) Nothing Empty))

findCat :: Tree (Maybe Cat) -> Int --Если кота нет, то напечатает 0
findCat a = findCat' a 0
        where findCat' Empty acc = acc
              findCat' (Leaf maybeCat) acc = if isJust maybeCat then acc + 1 else 0
              findCat' (Node leftTree maybeCat rightTree) acc | isJust maybeCat = acc
                                                              | otherwise = max (findCat' leftTree (acc + 1)) (findCat' rightTree (acc + 1))

findCat2 :: Tree (Maybe Cat) -> [Char] --Если кота нет, то напечатает ""
findCat2 a = findCat2' a []
        where findCat2' Empty acc = []
              findCat2' (Leaf maybeCat) acc = if isJust maybeCat then acc else []
              findCat2' (Node leftTree maybeCat rightTree) acc | isJust maybeCat = acc
                                                               | otherwise = maximumBy (compare`on`length) [(findCat2' leftTree (acc ++ ['L'])), (findCat2' rightTree (acc ++ ['R']))]

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

--task 6 (Histogram)
makeHistogram :: Show a => Eq a => [a] -> String
makeHistogram s = buildResultString (count (unique s) s) (length s)

unique :: Eq a => [a] -> [a]
unique s = unique' s []
unique' [] acc  = acc
unique' (x:xs) acc | (elem x acc) = unique' xs acc
                   | otherwise = unique' xs (acc ++ [x])

count :: Eq a => [a] -> [a] -> [(a, Int)]
count l s = map (\x -> (x, func x s) ) l
        where func :: Eq a => a -> [a] -> Int
              func n = foldl (\acc x -> if n == x then acc + 1 else acc) 0            

buildResultString :: Show a => [(a, Int)] -> Int -> String 
buildResultString allResults len = "Total: " ++ show len ++ " elem\n" ++ rest
        where rest :: String
              rest = unlines $ map makePrettyLine allResults
              makePrettyLine :: Show a => (a, Int) -> String
              makePrettyLine (el, n) = show el ++ " : " ++ makeSticks n 
              makeSticks :: Int -> String
              makeSticks x = take x $ repeat '|'

--task 7 (matrix)
transposition :: [[Char]] -> [[Char]]  
transposition m = [[m !! a !! b | a <- [0..(length (m) - 1)]] | b <- [0..(length (m !! 0) - 1)]]

--task 8
checker :: Int -> Bool
checker a = checker' a []
checker' a s | (a == 1) = True
             | (a `elem` s) = False
             | otherwise = checker' (summ_of_squares a 0) (a : s) 
        where summ_of_squares a acc | (a == 0) = acc
                                    | otherwise = summ_of_squares (a `div` 10) (acc + (a `mod` 10) ^ 2) 