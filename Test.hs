import System.IO
import Utils

--digitToInt --> machts beispielsweise 'F' --> 15 oder '9' --> 9, aber NICT 'L' --> ?

getFileNameFromPath :: [Char] -> [Char]
getFileNameFromPath []      = []
getFileNameFromPath (x:xs)  = (getFileNameFromPath xs) ++ [x]

isPrime :: Integer -> Bool
isPrime n = test (toInteger(toInt (sqrt $ toFloat n))) n

test :: Integer -> Integer -> Bool
test derzeit zahl
    | derzeit < 2               = True
    | mod zahl derzeit == 0     = False
    | otherwise                 = test (derzeit - 1) zahl

toInt :: Float -> Integer
toInt = round

toFloat :: Integer -> Float
toFloat x = read (show x) :: Float
