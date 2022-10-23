{-1-}
tobin :: Int -> Int
tobin 0 = 0
tobin x = 10 * tobin(x `div` 2) + (x `mod` 2)

{-2 Gorner's scheme-}
todec :: Int -> Int -> Int {-1 число основание системы счисления исходного числа, 2 число - исходное, 3 число - результат-}
todec n x = foldl (\acc x -> acc * n + x) 0 (lst x)
lst :: Int -> [Int]
lst 0 = []
lst x = lst (x `div` 10) ++ [x `mod` 10]

{-3-}
toint :: String -> Int
toint "" = 0
toint s = digit (last s) + (toint (init s) * 10)
digit x |(x == '0') = 0
        |(x == '1') = 1
        |(x == '2') = 2 
        |(x == '3') = 3
        |(x == '4') = 4
        |(x == '5') = 5
        |(x == '6') = 6
        |(x == '7') = 7
        |(x == '8') = 8
        |otherwise = 9

{-4-}
mysort :: [Int] -> [Int]
mysort [] = []
mysort (x:xs) = 
    let small = mysort [n | n <- xs, n <= x]
        big = mysort [n | n <- xs, n > x]
    in small ++ [x] ++ big

lost' :: [Int] -> Int --список чисел с пропущенным элементом -> пропущенный элемент
lost' s = lost (mysort s)
lost s | (head (tail s) - head s > 1) = (head s + 1)
       | otherwise = lost (tail s)

lost'' :: [Int] -> Int
lost'' s = 
    predicted_sum - real_sum
    where predicted_sum = ((1 + maximum s) * maximum s) `div` 2
          real_sum = sum s