module TestFunction (getTest, getTestDerivative) where
import Types
import GHC.Float (int2Float)

getTest :: Int -> Int -> Function
getTest n k = e_k

        where fn = int2Float n
              fk = int2Float k * 3 / fn

              e_k x | fk - 3/fn <= x && x < fk = fn/3 * x + 1 - fn * fk/3
                    | fk <= x && x < fk + 3/fn = -fn/3 * x + 1 + fn * fk/3
                    | otherwise = 0

getTestDerivative:: Int -> Int -> Function
getTestDerivative n k = de_k

         where fn = int2Float n
               fk = int2Float k * 3 / fn

               de_k x | fk - 3/fn <= x && x < fk = fn/3
                      | fk <= x && x < fk + 3/fn = -fn/3
                      | otherwise = 0

