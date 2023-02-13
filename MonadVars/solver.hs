type Variables = (Int, Int, Int)
data MonadVars a = MonadVars { actualComputation :: (Variables -> (a, Variables)) }

runComputation :: MonadVars a -> Variables -> (a, Variables)
runComputation computation vars = actualComputation computation $ vars

getVars :: MonadVars Variables
getVars = MonadVars $ \vars -> (vars, vars)

putVars :: Variables -> MonadVars ()
putVars vars2 = MonadVars $ \vars -> ((), vars2)

instance Functor MonadVars where
    -- fmap :: (a -> b) -> (MonadVars a) -> (MonadVars b)
    fmap f (MonadVars comp) =
        MonadVars $ \vars -> let (result, newVars) = comp vars in (f result, newVars)

instance Applicative MonadVars where
    pure x = MonadVars $ \vars -> (x, vars)
    --(<*>) :: MonadVars (a -> b) -> MonadVars a -> MonadVars b
    (MonadVars compF) <*> (MonadVars compX) = MonadVars $ \vars -> let (f, newVars) = compF vars in let (result, newVars2) = compX newVars in (f result, newVars2)

instance Monad MonadVars where
    --(>>=) :: MonadVars a -> (a -> MonadVars b) -> MonadVars b
    comp1 >>= comp2 = MonadVars $ \vars -> let (res, newVars) = runComputation comp1 vars in runComputation (comp2 res) newVars


computation :: MonadVars Int
computation = do
    (x1, x2, x3) <- getVars
    putVars (x1 + 2, x2 + 1, x3 * 3)
    return 200

main :: IO ()
main = do
    print $ runComputation computation (20, 30, 40)