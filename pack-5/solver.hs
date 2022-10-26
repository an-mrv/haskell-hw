--task 1
quadraticSolver :: Double -> Double -> Double -> Maybe (Double, Double)
quadraticSolver a b c | (d >= 0) = Just $ ((-b + sqrt d) / (2*a), (-b - sqrt d) / (2*a))
                      | otherwise = Nothing 
                    where d = b * b - 4*a*c

--task 2
maybeHead :: [a] -> Maybe a
maybeHead [] = Nothing
maybeHead (x:_) = Just $ x

maybeTail :: [a] -> Maybe [a]
maybeTail [] = Nothing
maybeTail (_:xs) = Just $ xs

maybeInit :: [a] -> Maybe [a]
maybeInit [] = Nothing
maybeInit(x:xs) = Just $ init' x xs
        where init' _ [] = []
              init' y (x:xs) = y : init' x xs

maybeFind :: (a -> Bool) -> [a] -> Maybe a
maybeFind _ []  = Nothing
maybeFind predicate s = find' predicate 0 s 
find' predicate len s | (len == length s) = Nothing
                      | predicate (s !! len) = Just $ (s !! len)
                      | otherwise = find' predicate (len + 1) s
                             
--task 3
data DogBreed = GoldenRetrievers
              | BostonTerriers
              | LabradorRetrievers
              | Poodles
              | BorderCollie
              | Beagle
              | IrishSetter
              | Staffordshire
              | Bull
              | Terrier
    deriving (Show, Eq)

data Gender = Male | Female
    deriving (Show, Eq)

data Dog = Dog { name :: String
               , age :: Int
               , gender :: Gender
               , breed :: DogBreed
               , isGoodBoy :: Bool -- holds for female dogs as well
               } deriving (Show, Eq)

dogs :: [Dog]
dogs = [ Dog "Leander" 12 Male Beagle False
       , Dog "Ouranos" 1 Male Poodles True
       , Dog "Pegasus" 2 Female Beagle False
       , Dog "Atlas" 8 Female GoldenRetrievers True
       , Dog "Castor" 6 Male LabradorRetrievers True
       , Dog "Apollo" 3 Female Beagle False
       , Dog "Narkissos" 15 Male Beagle True
       , Dog "Dardanos" 7 Female Terrier True
       , Dog "Ajax" 4 Male IrishSetter False
       , Dog "Pyrrhos" 2 Female BorderCollie False
       , Dog "Patroclus" 6 Male Bull True
       , Dog "Iacchus" 4 Female Beagle True ]

goodBoys :: [Dog]
goodBoys = goodBoys' [] 0
goodBoys' s len | (len == length dogs) = s
                | ((isGoodBoy $ (dogs !! len)) == True) = dogs !! len : goodBoys' s (len+1)
                | otherwise = goodBoys' s (len+1)

longNamedDogs :: [Dog]
longNamedDogs = longNamedDogs' [] 0
longNamedDogs' s len | (len == length dogs) = s
                     | (length(name $ (dogs !! len)) > 7) = dogs !! len : longNamedDogs' s (len+1)
                     | otherwise = longNamedDogs' s (len+1)

mostPopularDogGender :: Gender
mostPopularDogGender = mostPopularDogGender' 0 0 0
mostPopularDogGender' len m f | (len == length dogs) = if m > f then Male else Female
                              | ((gender $ (dogs !! len)) == Male) = mostPopularDogGender' (len + 1) (m + 1) f
                              | otherwise = mostPopularDogGender' (len + 1) m (f + 1)

averageDogAge :: Double
averageDogAge = averageDogAge' 0 0
averageDogAge' sum_age len | (len == length dogs) = (fromIntegral sum_age / fromIntegral len)
                           | otherwise = averageDogAge' (sum_age + (age $ (dogs !! len))) (len + fromInteger 1) 

dogsByBreed :: DogBreed -> [Dog]
dogsByBreed br = dogsByBreed' br [] 0
dogsByBreed' br s len | (len == length dogs) = s
                      | ((breed $ (dogs !! len)) == br) = dogs !! len : dogsByBreed' br s (len+1)
                      | otherwise = dogsByBreed' br s (len+1)

-- task 4.1

data Complex = Complex { real_part :: Double
                        , imaginary_part :: Double
                        } deriving (Show, Eq)

summ :: Complex -> Complex -> Complex
summ a b = Complex (real_part a + real_part b) (imaginary_part a + imaginary_part b)
--summ (Complex 1 2) (Complex 3 4)

diff :: Complex -> Complex -> Complex
diff a b = Complex (real_part a - real_part b) (imaginary_part a - imaginary_part b)

mult :: Complex -> Complex -> Complex
mult a b = Complex (real_part a * real_part b - imaginary_part a * imaginary_part b) (real_part a * imaginary_part b + real_part b * imaginary_part a)

divv :: Complex -> Complex -> Complex
divv a b = Complex ((real_part a * real_part b + imaginary_part a * imaginary_part b) / ((real_part b) ^ 2 + (imaginary_part b) ^ 2)) ((real_part b * imaginary_part a - real_part a * imaginary_part b) / ((real_part b) ^ 2 + (imaginary_part b) ^ 2))

conjugate :: Complex -> Complex
conjugate a = Complex (real_part a) (- imaginary_part a)

absolute_val :: Complex -> Double
absolute_val a = sqrt ((real_part a) ^ 2 + (imaginary_part a) ^ 2)

data MyList a = MyList { myhead :: a
                        , mytail :: [a]
                        } deriving (Show)

fromList :: [a] -> MyList a
fromList (x:xs) = MyList x xs

toList :: MyList a -> [a]
toList s = (myhead $ s) : (mytail $ s)

reverseMyList :: MyList a -> MyList a
reverseMyList s = MyList (last $ mytail $ s) (reverse' ((myhead $ s) : init (mytail $ s)))
                where reverse' = foldl (\acc x -> x : acc) []

mapMyList :: (a -> b) -> MyList a -> MyList b
mapMyList func s = MyList (func(myhead $ s)) ([func x | x <- (mytail $ s)])