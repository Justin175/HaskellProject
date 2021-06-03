module Utils
where 
import Data.Char

-- Konvertiert eine Liste von Integern zu einem String
-- Die einzelnen Elemente werden mit dem seperator getrennt
integerListToString :: [Integer] -> [Char] -> String
integerListToString liste seperator = integerListToString_ liste seperator False

integerListToString_ :: [Integer] -> [Char] -> Bool -> String
integerListToString_ [] _ _                           = []
integerListToString_ (x:xs) seperator addSepertator     | addSepertator = seperator ++ show x ++ (integerListToString_ xs seperator True)
                                                        | otherwise = show x ++ (integerListToString_ xs seperator True)

-- Konvertiert eine Liste aus Charaktären in eine Int-Liste
charListToAsciiIntList :: [Char] -> [Int]
charListToAsciiIntList list = map ord list

-- Konvertiert eine Liste aus Charaktären in eine Int-Liste
charListToAsciiIntegerList :: [Char] -> [Integer]
charListToAsciiIntegerList list = map (\z -> toInteger $ ord z) list

-- Konvertiert eine Int-Liste zu einer Integer-Liste
intListToIntegerList :: [Int] -> [Integer]
intListToIntegerList list = map toInteger list