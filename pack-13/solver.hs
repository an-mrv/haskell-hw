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
