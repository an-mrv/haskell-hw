array = ['a', 'b', 'c', 'd', 'e', 'f', 'g', 'h', 'i', 'j', 'k', 'l', 'm', 'n', 'o', 'p', 'q', 'r', 's', 't', 'u', 'v', 'w', 'x', 'y', 'z']
makestr :: Int -> Int -> Int -> Int -> [Char] -> [Char]
makestr pos1 pos2 letter all arr | ((pos1 == all) || (pos2 == all)) = makestr pos1 pos2 letter (all-1) ((array !! letter) : arr)
                                 | (all < 0) = arr
                                 | otherwise = makestr pos1 pos2 letter (all-1) ('*' : arr) 

func :: Int -> Int -> Int -> [Char] -> [Char]
func s a k arr | (a == 0) = arr
               | otherwise = func s (a - 1) k (arr ++ (makestr (a-1) (s - a) (k - a) (s - 1) []) ++ "\n")

func2 :: Int -> Int -> Int -> [Char] -> [Char]
func2 s a k arr | (a > k) = arr
                | (a == 1) = func2 s (a + 1) k arr
                | otherwise = func2 s (a + 1) k (arr ++ (makestr (a-1) (s - a) (k - a) (s - 1) []) ++ "\n")

makeArt :: Int -> IO()
makeArt a = putStrLn $ func (a*2 - 1) a a [] ++ func2 (a*2 - 1) 1 a []
