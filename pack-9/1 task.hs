import Prelude hiding (head, tail, maximum)
import Data.Maybe

type GreekData = [(String, [Integer])]
greekDataA :: GreekData
greekDataA = [ ("alpha", [5, 10])
             , ("beta", [0, 8])
             , ("gamma", [18, 47, 60])
             , ("delta", [42])
             ]

greekDataB :: GreekData
greekDataB = [ ("phi", [53, 13])
             , ("chi", [21, 8, 191])
             , ("psi", [])
             , ("omega", [6, 82, 144])
             ]

headMay :: [a] -> Maybe a
headMay [] = Nothing
headMay (x:xs) = Just x

divMay :: Double -> Double -> Maybe Double
divMay _ 0 = Nothing
divMay a b = Just $ a / b 

maximumMay :: (Ord a) => [a] -> Maybe a
maximumMay [] = Nothing 
maximumMay [x] = Just x
maximumMay (x:xs) | ((maximumMay xs) > Just x) = maximumMay xs
                  | otherwise = Just x

tailMay :: [a] -> Maybe [a]
tailMay [] = Nothing
tailMay (x:xs) = Just xs

{-
 tl;dr implement the function WITHOUT do-notation or any monad magic. only pattern-matching, where and let in

 first query the GreekData that is passed in,
 look up the string passed in the second argument,
 and retrieve the corresponding list of Integers. Call this list xs.
 Next calculate the maximum of the tail of xs
 (Don’t use any pattern matching here.
 Use case expressions and the maximumMay and tailMay functions)
 Take the maximum and divide it by the head of the list (using headMay and divMay functions).
 If any of these operations along the way return Nothing, then your function should return Nothing.
 But if everything succeeds, then return the final quotient.
 One hint… you’ll need to use the fromIntegral function to convert your two Integers to Doubles for the final call to divMay.
-}
queryGreek :: GreekData -> String -> Maybe Double
queryGreek a s = func (helper a s 0)
helper :: GreekData -> String -> Int -> [Integer]
helper a s n | (fst(a !! n) == s) = snd(a !! n)
             | otherwise = helper a s (n + 1)
func :: [Integer] -> Maybe Double
func xs | ((tailMay xs == Nothing) || ((maximumMay $ fromJust (tailMay xs)) == Nothing) || (headMay xs) == Nothing) = Nothing
        | otherwise = divMay (fromIntegral $ fromJust (maximumMay $ fromJust (tailMay xs))) (fromIntegral $ fromJust (headMay xs)) 

-- queryGreek greekDataA "alpha" == Just 2.0

-- Now do the same whole thing, but using do-notation, since Maybe is a Monad

queryGreekPro :: GreekData -> String -> Maybe Double
queryGreekPro a s = do
    xs <- Just $ helper a s 0
    h <- headMay xs
    t <- tailMay xs
    maxx <- maximumMay t
    divMay (fromIntegral maxx) (fromIntegral h)


-- * a harder task. rewrite queryGreekPro, but without the do-notation, only using the (>>=) operator and its friends
-- in other words, desugarize your notation
queryGreekProPlus :: GreekData -> String -> Maybe Double
queryGreekProPlus a s = divMay (fromIntegral $ fromJust((tailMay xs) >>= maximumMay)) (fromIntegral $ fromJust(headMay xs))
                where xs = helper a s 0