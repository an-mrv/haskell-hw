import Control.Monad.State
-- state monad

type RandState = [Int]

game :: State RandState String
game = do
    rollDice <- get
    let firstPlayerRes = head (rollDice) 
    put (tail (rollDice) ++ [head (rollDice)])
    rollDice <- get
    let secondPlayerRes = head (rollDice)
    put (tail (rollDice) ++ [head (rollDice)])
    if firstPlayerRes > secondPlayerRes
        then return("First wins")
        else return("Second wins")

runGame :: String
runGame = evalState game startSeed
    where startSeed = [4, 3, 5, 2, 1, 6]

--if startSeed = [3, 4, 5, 2, 1, 6] then "Second wins"
--if startSeed = [4, 3, 5, 2, 1, 6] then "First wins"