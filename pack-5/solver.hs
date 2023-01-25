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
maybeFind predicate (x:xs) | predicate x = Just $ x
                           | otherwise = maybeFind predicate xs
                             
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
goodBoys = [x | x <- dogs, isGoodBoy $ x]  

longNamedDogs :: [Dog]
longNamedDogs = [x | x <- dogs, (length $ name $ x) > 7]

mostPopularDogGender :: Gender
mostPopularDogGender = help (length [x | x <- dogs, gender x == Male]) (length [x | x <- dogs, gender x == Female])
                    where help m f | (m > f) = Male
                                   | otherwise = Female

averageDogAge :: Double
averageDogAge = fromIntegral (sum s) / fromIntegral (length s)
            where s = [age x | x <- dogs]

dogsByBreed :: DogBreed -> [Dog]
dogsByBreed br = [x | x <- dogs, breed x == br]

-- task 4.1

data Complex = Complex { real_part :: Double
                        , imaginary_part :: Double
                        } deriving (Show, Eq)

summ :: Complex -> Complex -> Complex
summ a b = Complex (real_part a + real_part b) (imaginary_part a + imaginary_part b)

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

data MyList a = Empty | Cons { listHead :: a, listTail :: MyList a}
    deriving (Show, Read, Eq, Ord)
-- 3 `Cons` (4 `Cons` (5 `Cons` Empty)) Пример вызова

fromList :: [a] -> MyList a
fromList [] = Empty
fromList (x:xs) = Cons x (fromList xs)

toList :: MyList a -> [a]
toList Empty = []
toList s = (listHead $ s) : (toList (listTail $ s))

reverseMyList :: MyList a -> MyList a
reverseMyList s = reverse' (toList s)
                where reverse' s = fromList(foldl (\acc x -> x : acc) [] s)

mapMyList :: (a -> b) -> MyList a -> MyList b
mapMyList _ Empty = Empty
mapMyList func s = Cons (func (listHead $ s)) (mapMyList func (listTail $ s))
-- Пример вызова: mapMyList (\x -> x + 5) Cons {listHead = 3, listTail = Cons {listHead = 4, listTail = Cons {listHead = 5, listTail = Empty}}}

{-data MyList a = MyList { myhead :: a
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
mapMyList func s = MyList (func(myhead $ s)) ([func x | x <- (mytail $ s)])-}