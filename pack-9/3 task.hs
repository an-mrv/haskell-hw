import Control.Monad.State

-- see this program as example:
{-
x=foo
y=bar
y=$x
l=l
x=$y
-}
{-
At the end of this program the state is:
x = foo
y = foo
l = l
-}

exampleProgram :: String
exampleProgram = "x=foo\ny=bar\ny=$x\nl=l\nx=$y"

-- one of possible answers. order in list doesn't matter
exampleAns :: [(String, String)]
exampleAns = [("x", "foo"), ("y", "foo"), ("l", "l")]

check :: IO ()
check = do
    let resultState = solveState exampleProgram
    if exampleAns `listEq` resultState
        then putStrLn "OK!"
        else error "something wrong:("
    where listEq l r = leftInRight && rightInLeft
            where leftInRight = all (\x -> x `elem` r) l
                  rightInLeft = all (\x -> x `elem` l) r

data Value = Literal String | VariableReference String deriving Show
data Command = Command { varName :: String, whatToPut :: Value } deriving Show

getValue :: Value -> String
getValue (Literal a) = a
getValue (VariableReference a) = a

isLiteral :: Value -> Bool
isLiteral (Literal a) = True
isLiteral (VariableReference a) = False

-- you can choose something else!
type InterpreterState = [(String, String)]

solveState :: String -> [(String, String)]
solveState input = interpretToState (map parse $ lines input)

-- example: "foo=bar" -> Command "foo" (Literal "bar")
-- example: "foo=$bar" -> Command "foo" (VariableReference "bar")
parse :: String -> Command
parse x = helper x "" "" 0
    where helper :: String -> String -> String -> Int -> Command
          helper s name val f | ((s == []) && (not('$' `elem` val))) = Command {varName = name, whatToPut = Literal val}
                              | ((s == []) && ('$' `elem` val)) = Command {varName = name, whatToPut = VariableReference (remove val)}
                              | (head s == '=') = helper (tail s) name val 1
                              | (f == 0) = helper (tail s) (name ++ [head s]) val f
                              | otherwise = helper (tail s) name (val ++ [head s]) f
            where remove :: String -> String --remove $ from string
                  remove = foldr(\x acc -> if x /= '$' then x : acc else acc) []

-- you may rewrite this. e.g. you can use fold
-- but if you look at standard library there might be
-- a better alternative for chaining state functions.
-- In other words, executing a list of (State s a)
-- functions is a common task, and it has a standard implementation
interpretMany :: [Command] -> State InterpreterState ()
interpretMany [] = return ()
interpretMany (x:xs) = do
    interpretOne x
    interpretMany xs

-- using get, set and other State functions, interpret the command
interpretOne :: Command -> State InterpreterState ()
interpretOne a = do
    s <- get
    if ((find_name s (varName a)) && (isLiteral (whatToPut a))) 
        then put (change_val s (varName a) (getValue(whatToPut a)) [])
        else 
            if ((not(find_name s (varName a))) && (isLiteral (whatToPut a)))
                then put (s ++ [(varName a, (getValue (whatToPut a)))])
                else
                    if ((not(find_name s (varName a))) && not(isLiteral (whatToPut a)))
                        then put (s ++ [(varName a, find_val s (getValue (whatToPut a)))])
                        else
                            put (change_val s (varName a) (find_val s (getValue(whatToPut a))) [])
        

find_name :: InterpreterState -> String -> Bool
find_name s name | (s == []) = False
                 | (fst(head s) == name) = True
                 | otherwise = find_name (tail s) name

find_val :: InterpreterState -> String -> String
find_val s name | (fst(head s) == name) = snd(head s)
                | otherwise = find_val (tail s) name

change_val :: InterpreterState -> String -> String -> InterpreterState -> InterpreterState
change_val s name val res | (s == []) = res
                          | (fst(head s) == name) = change_val (tail s) name val (res ++ [(name, val)])
                          | otherwise = change_val (tail s) name val (res ++ [(fst(head s), snd(head s))])

-- you can choose other type for result
interpretToState :: [Command] -> [(String, String)]
interpretToState commands = execState (interpretMany commands) emptyState
    where emptyState = []