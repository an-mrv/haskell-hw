module Barans (
Sheep,
names,
father,
mother
) where


import Control.Monad
import Data.List

data Tree a = Leaf a | Branch (Tree a) a (Tree a)
type TreeList a = [Tree a]

fringe (Leaf x) = [x]
fringe (Branch left _ right) = fringe left ++ fringe right

kolvo :: Tree a -> Int
kolvo (Leaf _ ) = 1
kolvo (Branch l _ r) = kolvo l + kolvo r + 1

leftA :: Tree a -> Maybe (Tree a)
leftA (Leaf _)       = Nothing
leftA (Branch l _ r) = Just l

rightA :: Tree a -> Maybe (Tree a)
rightA (Leaf _)       = Nothing
rightA (Branch l _ r) = Just r

content :: Tree a -> a
content (Leaf x)       = x
content (Branch _ x _) = x

type Sheep = String

mother' :: Sheep -> Tree Sheep -> Maybe Sheep
mother' _ (Leaf _) = Nothing
mother' s (Branch l c r) = if (s == c) 
                            then Just (content l)  
                            else if (mother' s l) == Nothing then mother' s r else mother' s l

mother'' :: Sheep -> TreeList Sheep -> Maybe Sheep 
mother'' _ [] = Nothing
mother'' s (x:xs) = (mother' s x) `mplus` mother'' s xs

mother s = mother'' s [i10, i12]

father' :: Sheep -> Tree Sheep -> Maybe Sheep
father' _ (Leaf _) = Nothing
father' s (Branch l c r) = if (s == c) 
                            then Just (content r)  
                            else (father' s l) `mplus` (father' s r)

father'' :: Sheep -> TreeList Sheep -> Maybe Sheep 
father'' _ [] = Nothing
father'' s (x:xs) = (father' s x) `mplus` (father'' s xs)

father s = father'' s [i10, i12]

names' :: Tree Sheep -> [Sheep]
names' (Leaf x)       = [x]
names' (Branch l x r) = (names' l) ++ [x] ++ (names' r)

names'' :: TreeList Sheep -> [Sheep]
names'' [] = []
names'' (x:xs) = (names' x) `mplus` (names'' xs)

names = (sort . nub . names'') [i10, i12]

i8  = Branch (Branch (Leaf "i1") "i3" (Leaf "i2")) "i8" (Leaf "i7")
i9  = Branch (Leaf "i3") "i9" (Leaf "i5")
i10 = Branch i8 "i10" i9
i11 = Branch i8 "i11" i9
i6  = Branch (Leaf "i4") "i6" (Leaf "i5")
i12 = Branch i11 "i12" i6

{--
                      i12
          i10, i11
      i8           i9         i6
   i3    i7     i3   i5    i4    i5
i1    i2

--}

fromJust :: Maybe a -> a
fromJust (Just x) = x

find_grandpa :: Sheep -> Maybe Sheep 
find_grandpa s = mother s >>= father

find_grandgrandpa :: Sheep -> Maybe Sheep 
find_grandgrandpa s = find_grandpa s >>= father

parents :: Sheep -> [Maybe Sheep]
parents s = [mother s, father s] 

grandparents :: Sheep -> [Maybe Sheep]
grandparents s | (mother s == Nothing && father s == Nothing) = [Nothing, Nothing, Nothing, Nothing]
               | (mother s == Nothing) = [Nothing, Nothing] ++ parents (fromJust $ father s)
               | (father s == Nothing) = parents (fromJust $ mother s) ++ [Nothing, Nothing]
               | otherwise = parents (fromJust $ mother s) ++ parents (fromJust $ father s)
               
orphan :: Sheep -> String
orphan s = if (mother s == Nothing && father s == Nothing) then "Yes" else "No"

selected_barans = ["i3", "i5", "i6", "i9", "i12"]

selected_father :: Sheep -> Maybe Sheep
selected_father s = do
    a <- father s
    if a `elem` selected_barans then return (a) else Nothing

near_selected_father :: Sheep -> Maybe Sheep
near_selected_father s | (father s == Nothing) = Nothing
                       | (selected_father s /= Nothing) = selected_father s
                       | otherwise = father s >>= near_selected_father