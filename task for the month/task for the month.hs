{-1-}
inc :: Int -> Int
inc x | x >= 0 = x + 1
      | otherwise = error "Arg must be positive!"
dec :: Int -> Int
dec x | x > 0 = x - 1
      | x == 0 = 0
      | otherwise = error "Arg must be positive!"

pls :: Int -> Int -> Int
pls 0 y = y
pls x y = pls (dec x) (inc y)

mns :: Int -> Int -> Int
mns x 0 = x
mns x y = if x < y then 0 else mns (dec x) (dec y)

mlt :: Int -> Int -> Int
mlt x y = if (x < 0 || y < 0) then error "Arg must be positive!" else helper x x (y-1)
helper s x 0 = x
helper s x y = helper s (x + s) (dec y)

{-2-}
maxx :: Int -> Int -> Int
maxx x y = helper' x x y y
helper' 0 x acc y = y
helper' acc x 0 y = x
helper' acc1 x acc2 y = helper' (dec acc1) x (dec acc2) y

minn :: Int -> Int -> Int
minn x y = helper'' x x y y
helper'' 0 x acc y = x
helper'' acc x 0 y = y
helper'' acc1 x acc2 y = helper'' (dec acc1) x (dec acc2) y

{-3-}
{-co-recursion-}
divv1 :: Int -> Int -> Int
divv1 x y | (x < 0 || y < 0) = error "Arg must be positive!"
          | (y == 0) = 0
          | otherwise = solver 0 0 x y
solver acc1 acc2 x y | (acc2 == x) = acc1
                     | (acc2 > x) = (acc1 - 1)
                     | otherwise = solver (inc acc1) (pls acc2 y) x y

{-4-}
{-recurtion-}
divv2 :: Int -> Int -> Int
divv2 x y | (x < 0 || y < 0) = error "Arg must be positive!"
          | (y == 0) = 0
          | otherwise = solver' 0 x y
solver' acc x y | (x < y) = acc
                | otherwise = solver' (inc acc) (mns x y) y

{-5-}
{-co-recursion-}
modd1 :: Int -> Int -> Int
modd1 x y | (x < 0 || y < 0) = error "Arg must be positive!"
          | (y == 0) = 0
          | otherwise = solver'' 0 x y
solver'' acc x y | (acc == x) = 0
                 | (acc > x) = (mns x (mns acc y))
                 | otherwise = solver'' (pls acc y) x y

{-recurtion-}
modd2 :: Int -> Int -> Int
modd2 x y | (x < 0 || y < 0) = error "Arg must be positive!"
          | (y == 0) = 0
          | otherwise = solverr x y
solverr x y | (x == 0) = 0
            | (x < y) = x
            | otherwise = solverr (mns x y) y

{-6-}
whole_div :: Int -> Int -> Bool
whole_div x y | (x < 0 || y < 0) = error "Arg must be positive!"
              | (y == 0) = False
              | otherwise = help x y
help x y | (x == 0) = True
         | (x < y) = False
         | otherwise = help (mns x y) y

{-7-}
nd :: Int -> Int
nd x | (x < 0) = error "Arg must be positive!"
     | otherwise = nd' 0 1 x
nd' acc1 acc2 x | (acc2 > x) = acc1
                | (whole_div x acc2) = nd' (acc1 + 1) (acc2 + 1) x
                | otherwise = nd' acc1 (acc2 + 1) x
            
{-8-}
sumd :: Int -> Int
sumd x | (x < 0) = error "Arg must be positive!"
       | otherwise = sumd' 0 1 x
sumd' acc1 acc2 x | (acc2 > x) = acc1
                  | (whole_div x acc2) = sumd' (acc1 + acc2) (acc2 + 1) x
                  | otherwise = sumd' acc1 (acc2 + 1) x 

{-9-}
prime :: Int -> Bool
prime x | (x == 0 || x == 1) = False
        | (x < 0) = error "Arg must be positive!"
        | ((nd x) == 2) = True
        | otherwise = False

{-10-}
pnd :: Int -> Int
pnd x | (x < 0) = error "Arg must be positive!"
      | otherwise = pnd' 0 1 x
pnd' acc1 acc2 x | (acc2 > x) = acc1
                 | ((whole_div x acc2) && (prime acc2)) = pnd' (acc1 + 1) (acc2 + 1) x
                 | otherwise = pnd' acc1 (acc2 + 1) x

{-11-}
nod :: Int -> Int -> Int
nod x y | (x < 0 || y < 0) = error "Arg must be positive!"
        | otherwise = nod' (minn x y) x y
nod' acc x y | (x `mod` acc == 0 && y `mod` acc == 0) = acc
             | otherwise = nod' (acc - 1) x y

{-12-}
nok :: Int -> Int -> Int
nok x y | (x < 0 || y < 0) = error "Arg must be positive!"
        | otherwise = nok' (maxx x y) x y
nok' acc x y | (acc `mod` x == 0 && acc `mod` y == 0) = acc
             | otherwise = nok' (acc + 1) x y

{-13-}
{-1 argument-}
minimizationOperator :: (Int -> Int -> Int) -> Int -> Int -> Int
minimizationOperator f x t | ((f t x) == 0) = t
                           | (t == 1000000) = error "function not defined"
                           | otherwise = minimizationOperator f x (inc t)
                        
f x = minimizationOperator (\t x -> x - t) x 0
g x = minimizationOperator (\t x -> x + t) x 0

{-2 arguments-}
minimizationOperator' :: (Int -> Int -> Int -> Int) -> Int -> Int -> Int -> Int
minimizationOperator' n x1 x2 t | ((n t x1 x2) == 0) = t
                                | (t == 1000000) = error "function not defined"
                                | otherwise = minimizationOperator' n x1 x2 (inc t)
                        
f' x1 x2 = minimizationOperator' (\t x1 x2 -> x1 + x2 - t) x1 x2 0
g' x1 x2 = minimizationOperator' (\t x1 x2 -> x1 + x2 + t) x1 x2 0

{-14-}
{-1 argument-}
minimizationOperator'' :: (Int -> Int -> Bool) -> Int -> Int -> Int
minimizationOperator'' f x t | ((f x t) == True) = t
                             | (t == 1000000) = error "function not defined"
                             | otherwise = minimizationOperator'' f x (inc t)

r x = minimizationOperator'' (\x t -> (x < (t + 1)^2)) x 0

{-2 arguments-}
minimizationOperator''' :: (Int -> Int -> Int -> Bool) -> Int -> Int -> Int -> Int
minimizationOperator''' f x1 x2 t | ((f x1 x2 t) == True) = t
                                  | (t == 1000000) = error "function not defined"
                                  | otherwise = minimizationOperator''' f x1 x2 (inc t)

r' x1 x2 = minimizationOperator''' (\x1 x2 t -> (x1 < (t + 1)*x2)) x1 x2 0


{-15-}
squareroot :: Int -> Int 
squareroot x = minimizationOperator'' (\x t -> (x < (t + 1)^2)) x 0

{-16-}
division :: Int -> Int -> Int
division x1 x2 = minimizationOperator''' (\x1 x2 t -> (x1 < (t + 1)*x2)) x1 x2 0