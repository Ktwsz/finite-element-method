module Main where

import Data.Matrix
import GHC.List as Ghcl (concat)
import System.Environment
import GHC.Float (int2Float)
import System.Directory (createDirectoryIfMissing)

import Integral (integrate)
import Types
import TestFunction (getTest, getTestDerivative)


directory :: FilePath
directory = "wynik/"

wFile :: FilePath
wFile = "wagi.txt"

fFile :: FilePath
fFile = "wynik.txt"


omega :: Domain
omega = (0, 3)

constRho :: Function
constRho x | 0 <= x && x < 1 = 0
           | 1 <= x && x < 2 = 1
           | 2 <= x && x <= 3 = 0

constPhi :: Function
constPhi x = 5 - x / 3

constPI :: Float
constPI = 3.1415

constG :: Float
constG = 5.6743


getBMatrix :: Int -> Matrix Float
getBMatrix n = scaleMatrix (-1.0) .
                    (<$>) (integrate n omega) .
                    fromList (n-1) (n-1) .
                    Ghcl.concat $
                    [[b i j | i <- [1..n-1]] | j <- [1..n-1]]
    where b i j = \x -> getTestDerivative n i x * getTestDerivative n j x 

getLMatrix :: Int -> Matrix Float
getLMatrix n = (<$>) (integrate n omega) .
                    fromList (n-1) 1 $ 
                    [l i | i <- [1..n-1]]
    where l i = \x -> 4 * constG * constPI * getTest n i x * constRho x


solve :: Int -> Either String [Float]
solve n = let invertedBMatrix = inverse $ getBMatrix n
              lMatrix = getLMatrix n 
          in do
            m <- invertedBMatrix
            return . toList $ multStd m lMatrix 


getFunctionValues :: Int -> Domain -> [Float]
getFunctionValues n (a, b) = map (\x -> (x + 0.5) / int2Float n * (b - a) + a) [0..int2Float n - 1]


getPhiFunction :: Int -> [Float] -> Function
getPhiFunction n weights = let base = zipWith (\f w -> (\x -> w * f x)) [getTest n i | i <- [1..n-1]] weights

                           in \x -> constPhi x + (sum . map ($ x) $ base)


showOutput :: Show a => [a] -> String
showOutput = Ghcl.concat . map (\x -> show x ++ "\n") 

createAndWrite :: Show a => FilePath -> FilePath -> [a] -> IO ()
createAndWrite dir name content = do
    createDirectoryIfMissing False dir

    writeFile (dir ++ "/" ++ name) . showOutput $ content


main :: IO ()
main = do
    args <- getArgs
    if null args
        then do
            putStrLn "Nie podano n."
    else do
        let n = read (head args) :: Int
        let solution = solve n
        case solution of
            Left err -> putStrLn err
            Right weights -> do 
                                createAndWrite directory wFile weights
                                let values = getFunctionValues n omega
                                let phi = getPhiFunction n weights
                                
                                createAndWrite directory fFile . zip values $ map phi values
