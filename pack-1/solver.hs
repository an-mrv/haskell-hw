solver :: Double -> Double -> Double -> (Double, Double)
solver a b c | (d >= 0) = ((-b + sqrt d) / (2*a), (-b - sqrt d) / (2*a))
             | otherwise = error "No solution"
            where d = b * b - 4*a*c