module Integral (integrate) where
import Types
import GHC.Float (int2Float)

gaussIntegral :: [(Float, Float)] -> Function -> Float
gaussIntegral weights f  = sum .
                            map (\(w, x) -> w * f x) $
                            weights


twoPoint :: [(Float, Float)]
twoPoint = [(1, -1 / sqrt 3), (1, 1 / sqrt 3)]

twoPointIntegral :: Function -> Float
twoPointIntegral = gaussIntegral twoPoint

mapCoordinates :: Domain -> Function -> Function
mapCoordinates (a, b) f = \x -> intervalLength * f (intervalLength * x + offset)

    where intervalLength = (b - a) / 2
          offset = (a + b) / 2


partialIntegral :: Domain -> Function -> Float
partialIntegral domain = twoPointIntegral . mapCoordinates domain 

integrate :: Int -> Domain -> Function -> Float
integrate n (da, db) f = sum .
                        map (\x -> partialIntegral x f) $
                        domainDivided
    where fn = int2Float n 
          domainSize = db - da
          domainDivided = [(int2Float i * (domainSize / fn), int2Float (i + 1) * (domainSize / fn)) | i <- [0..n-1]]
