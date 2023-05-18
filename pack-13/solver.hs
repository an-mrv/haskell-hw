import Control.Monad.State
import Control.Monad.Trans.Maybe
import Control.Monad.Trans.Writer
import qualified Data.Map as MP

--task 1

-- fix this!!!
solveQ :: Double -> Double -> Double -> Maybe (Double, Double) 
solveQ a b c | (d >= 0) = Just $ ((-b + sqrt d) / (2*a), (-b - sqrt d) / (2*a))
             | otherwise = Nothing 
        where d = b * b - 4*a*c

-- fix this code.

solveUserQ :: MaybeT IO (Double, Double)
solveUserQ = do
  input <- lift getLine -- (1,2,3)
  let (a, b, c) = read input :: (Double, Double, Double)
  r <- (MaybeT . pure) $ solveQ a b c
  return r

--task 2

type Log = [String]
data UserInfo = UserInfo { address :: String, name :: String, salary :: Int } deriving (Show)

makeSureUserIsComfortableGivingInformation :: String -> MaybeT IO ()
makeSureUserIsComfortableGivingInformation infoName = do
  lift $ putStr $ "Are you ok sharing " ++ infoName ++ "?\n"
  input <- lift getLine
  guard (input == "yes") 
  return ()

--runMaybeT $ makeSureUserIsComfortableGivingInformation "smth"

-- example purposes. This MaybeT function always fails
nothingExample :: MaybeT IO ()
nothingExample = do
    guard False

-- you can run this using runGetUserInfo
getUserInfo :: WriterT Log (MaybeT IO) UserInfo
getUserInfo = do
  n <- getUserName
  tell ["User has responded with yes"]
  ad <- getUserAddress
  tell ["User has responded with yes"]
  s <- getUserSalary
  tell ["User has responded with yes"]
  return $ UserInfo ad n s

getUserSalary :: WriterT Log (MaybeT IO) Int
getUserSalary = do
  answ <- lift $ makeSureUserIsComfortableGivingInformation "salary"
  (lift . lift) $ putStrLn "Please, share "
  input <- (lift . lift) getLine
  let s = read input :: Int
  return $ s
  
getUserName :: WriterT Log (MaybeT IO) String
getUserName = do
  answ <- lift $ makeSureUserIsComfortableGivingInformation "name"
  (lift . lift) $ putStrLn "Please, share "
  input <- (lift . lift) getLine
  return $ input

getUserAddress :: WriterT Log (MaybeT IO) String
getUserAddress = do
  answ <- lift $ makeSureUserIsComfortableGivingInformation "address"
  (lift . lift) $ putStrLn "Please, share "
  input <- (lift . lift) getLine
  return $ input

runGetUserInfo = do
  res <- runMaybeT $ runWriterT getUserInfo
  print res