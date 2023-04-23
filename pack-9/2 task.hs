import Control.Monad.State
-- state monad

type RandState = Int

array :: [Int]
array = [4, 3, 5, 2, 1, 6]

rollDice :: [Int] -> Int -> Int
rollDice arr n = arr !! (n-1) 

game :: State RandState String
game = do
    a <- get
    let firstPlayerRes = rollDice array a
    put (firstPlayerRes)
    b <- get
    let secondPlayerRes = rollDice array b
    put (secondPlayerRes)
    if firstPlayerRes > secondPlayerRes
        then return("First wins")
        else return("Second wins")

runGame :: String
runGame = evalState game startSeed
    where startSeed = 5
