--task1
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



-- examples
-- how many dogs are of age 2, 4 and 6?

dogsAge246 :: [(Dog, Dog, Dog)]
dogsAge246 = do
    dogsAge2 <- dogsAge 2
    dogsAge4 <- dogsAge 4
    dogsAge6 <- dogsAge 6
    return (dogsAge2, dogsAge4, dogsAge6)
    where dogsAge n = filter (\dog -> age dog == n) dogs


-- using do-notation, find such dogs, that they are male, 4-5 years old, not IrishSetter, good boys
-- and such dogs, that they are female, 4-5 years old, name is longer then 4 symbols
-- after finding those two groups, combine a list of all combinations they could be mated

dogs1 :: [Dog]
dogs1 = filter (\dog -> (gender dog == Male) && (age dog == 4 || age dog == 5) && (breed dog /= IrishSetter) && (isGoodBoy dog == True)) dogs

dogs2 :: [Dog]
dogs2 = filter (\dog -> (gender dog == Female) && (age dog == 4 || age dog == 5) && (length (name dog) > 4)) dogs
                      
helperr' :: [Dog] -> [Dog] -> Int -> [[(Dog, Dog)]] -> [[(Dog, Dog)]] --Если a <= b
helperr' a b countB res | (countB == (length b)) = res
                        | otherwise = helperr' a b (countB + 1) ((helperr a b 0 countB (length a) (length b) []) : res)
                        where helperr :: [Dog] -> [Dog] -> Int -> Int -> Int -> Int -> [(Dog, Dog)] -> [(Dog, Dog)]
                              helperr a b countA countB lenA lenB res | (countA == lenA) = res
                                                                      | (countB == lenB) = helperr a b countA 0 lenA lenB res
                                                                      | otherwise = helperr a b (countA+1) (countB+1) lenA lenB ((a !! countA, b !! countB) : res)
                                  
dogsQuery :: [[(Dog, Dog)]]
dogsQuery = do
   a <- dogs1
   b <- dogs2
   c <- if (length dogs1) <= (length dogs2) then helperr' dogs1 dogs2 0 [] else helperr' dogs2 dogs1 0 []
   return (c)

--task2

makeLine :: Int -> Int -> (Int, Int) -> (Int, Int) -> String -> String
makeLine countX countY a b res | (countY == 8) = res ++ "\n"
                               | (fst a == countX && snd a == countY) = makeLine countX (countY+1) a b (res ++ "Q ")
                               | (fst b == countX && snd b == countY) = makeLine countX (countY+1) a b (res ++ "q ")
                               | otherwise = makeLine countX (countY+1) a b (res ++ "_ ")

helper :: (Int, Int) -> (Int, Int) -> Int -> String -> String
helper a b countX res | (countX == 8) = res
                      | otherwise = helper a b (countX + 1) (res ++ makeLine countX 0 a b [])

prettyBoard :: (Int, Int) -> (Int, Int) -> IO()
prettyBoard a b = putStrLn $ helper a b 0 []

canAttack :: (Int, Int) -> (Int, Int) -> Bool
canAttack (x1, y1) (x2, y2) | ((x1 == x2) || (y1 == y2) || (x1 - y1 == x2 - y2) || (x1 + y1 == x2 + y2)) = True
                            | otherwise = False