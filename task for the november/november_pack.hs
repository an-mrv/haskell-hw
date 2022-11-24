--task 1
(!!!) :: [a] -> Int -> a
(!!!) [] _ = error "Empty list"
(!!!) (x:xs) n | (n == 0) = x
               | (n < 0 || n >= length (x:xs)) = error "Index out of range"   
               | otherwise = (!!!) xs (n - 1)

--task 2
init' :: [a] -> [a]
init' [] = error "Empty list"
init' (x:[]) = []
init' (x:xs) = x : init' xs

--task 3
(+++) :: [a] -> [a] -> [a]
(+++) [] s = s
(+++) x s = helper x s (length x - 1)
    where helper (x:xs) s 0 = x : s 
          helper x s ind = helper (init' x) ((x !!! ind) : s) (ind - 1)

--task 4
cycle' :: [a] -> [a]
cycle' [] = []
cycle' s = cycle'' s
    where cycle'' s = s ++ cycle'' s

--task 5
take' :: Int -> [a] -> [a] 
take' _ [] = []
take' n s = take'' 0 n s
take'' c n [] = error "No solutions"
take'' c n (x:xs) | (n == c) = []
                  | otherwise = x : (take'' (c + 1) n xs)

--task 6
inits :: [a] -> [[a]]
inits [] = [[]]
inits (x:[]) = [[]] +++ [[x]]
inits s = inits (init' s) +++ [s]

tails :: [a] -> [[a]]
tails [] = [[]]
tails (x:[]) = [[x]] +++ [[]]
tails (x:xs) = [(x:xs)] +++ tails xs

--task 7
elem' :: Eq a => a -> [a] -> Bool
elem' _ [] = False
elem' n (x:xs) | (n == x) = True
               | otherwise = elem' n xs

--task 8
nub :: Eq a => [a] -> [a]
nub s = nub' s []
nub' [] acc  = acc
nub' (x:xs) acc | (elem x acc) = nub' xs acc
                | otherwise = nub' xs (x : acc)

--task 9
updElmBy :: [a] -> Int -> a -> [a]
updElmBy [] ind el = error "Empty list"
updElmBy s ind el | ((length s - 1) < ind || ind < 0) = error "No solutions" 
                  | otherwise = take' ind s +++ [el] +++ drop (ind + 1) s

--task 10
swp :: [a] -> Int -> Int -> [a]
swp [] ind1 ind2 = error "Empty list"
swp s ind1 ind2 | ((length s - 1) < ind1 || (length s - 1) < ind2 || ind1 < 0 || ind2 < 0) = error "No solutions" 
                | (ind1 == ind2) = s
                | otherwise = swp' s (min ind1 ind2) (max ind1 ind2)
swp' s ind1 ind2 = take ind1 s +++ [s !!! ind2] +++ helper' s (ind1 + 1) (ind2 - 1) [] +++ [s !!! ind1] +++ drop (ind2 + 1) s
    where helper' s ind1 ind2 acc | (ind1 > ind2) = acc
                                  | otherwise = helper' s (ind1 + 1) ind2 (acc +++ [s !!! ind1])

--task 11
permutations :: [a] -> [[a]]
permutations [] = [[]]
permutations s = [x : xs | (x, ys) <- permutations' s, xs <- permutations ys]
    where permutations' :: [a] -> [(a, [a])]
          permutations' [] = []
          permutations' (x:xs) = (x, xs) : map (\(y, ys) -> (y, x : ys)) (permutations' xs)

--task 12
subsequences ::[a] -> [[a]]
subsequences [] = [[]]
subsequences (x:xs) = subsequences xs +++ [x:y | y <- subsequences xs]

--task 13
cubsum :: [Int] -> Int
cubsum = foldr (\x acc -> x^3 + acc) 0

--task 14
cubsum' :: [Int] -> Int
cubsum' = foldl (\acc x -> x^3 + acc) 0

--task 15
expT :: Double -> Int -> Double
expT x n = expT' (reverse $ array_for_fact n) x
expT' s y = foldl (\acc x -> acc + (y ^ x / fromIntegral (fact x))) 1.0 s

fact :: Int -> Int
fact n = fact' (array_for_fact n)
    where fact' = foldl (\acc x -> acc * x) 1

array_for_fact :: Int -> [Int]
array_for_fact 0 = []
array_for_fact n = n : (array_for_fact (n - 1))

--task 16
howmany :: Eq a => a -> [a] -> Int
howmany el = foldl (\acc x -> if (x == el) then acc + 1 else acc) 0

--task 17
howmany_g_b_letters :: [Char] -> (Int,Int)
howmany_g_b_letters = foldl (\(a, b) x -> if elem' x g_letters then (a+1, b) else if elem' x b_letters then (a, b+1) else (a, b)) (0, 0)

g_letters = ['a','e','i','o','u']
b_letters = ['t','n','r','s','h']

--task 18
intersperse :: a -> [a] -> [a]
intersperse a = foldl (\acc x  -> if null acc then x : acc else acc +++ [a, x]) []

--task 19
--foldl
cycleshift (x:xs) = xs ++ [x]

rotate :: [a] -> [[a]]
rotate s = rotate' s (list_of_ind $ length s)
rotate' s = foldl (\acc x -> (cycleshift' s x) : acc) []

cycleshift' :: [a] -> Int -> [a]
cycleshift' s 0 = s
cycleshift' s n = cycleshift' (cycleshift s) (n-1)

list_of_ind :: Int -> [Int]
list_of_ind 0 = []
list_of_ind n = list_of_ind (n-1) +++ [n-1]

--foldr
rotate2 :: [a] -> [[a]]
rotate2 s = rotate2' s (reverse $ list_of_ind $ length s)
rotate2' s = foldr (\x acc -> (cycleshift' s x) : acc) []