{-head-}
myhead :: [Int] -> Int
myhead [] = error "No"
myhead (x:xs) = x

{-tail-}
mytail :: [Int] -> [Int]
mytail [] = error "No"
mytail (x:xs) = xs

{-last-}
mylast :: [Int] -> Int
mylast [] = error "No"
mylast (x:[]) = x
mylast (x:xs) = mylast xs

{-init-}
myinit :: [Int] -> [Int]
myinit [] = error "No"
myinit (x:[]) = []
myinit(x:xs) = x : myinit xs

{-length-}
mylength :: [Int] -> Int
mylength [] = 0
mylength (x:[]) = 1
mylength (x:xs) = 1 + mylength xs

{-null-}
mynull :: [Int] -> Bool
mynull [] = True
mynull (x:_) = False

{-drop-}
mydrop :: Int -> [Int] -> [Int]
mydrop _ [] = []
mydrop 0 (x:xs) = (x:xs)
mydrop n (x:xs) = mydrop (pred n) xs

{-sum-}
mysum :: [Int] -> Int
mysum [] = 0
mysum (x:[]) = x
mysum (x:xs) = x + mysum xs

{-product-}
myproduct :: [Int] -> Int
myproduct [] = 1
myproduct (x:[]) = x
myproduct (x:xs) = x * myproduct xs

{-elem-}
myelem :: Int -> [Int] -> Bool
myelem _ [] = False
myelem n (x:xs) | (n == x) = True
                | otherwise = myelem n xs

{-reverse-}
myreverse :: [Int] -> [Int]
myreverse [] = []
myreverse (x:[]) = [x]
myreverse (x:xs) = myreverse xs ++ [x]