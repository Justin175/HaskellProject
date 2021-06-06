module Utils
where 
import Data.Char

-- Gibt den öffentlichen Schlüssel aus einer Liste als Tuple zurück
getPublicKeyFromList :: [Integer] -> (Integer, Integer)
getPublicKeyFromList [p, n] = (p, n)

-- Gibt den privaten Schlüssel aus einer Liste als Tuple zurück
getPrivateKeyFromList :: [Integer] -> (Integer, Integer)
getPrivateKeyFromList [p, n] = (p, n)

-- Konvertiert eine Liste von Integern zu einem String
-- Die einzelnen Elemente werden mit dem seperator getrennt
integerListToString :: [Integer] -> [Char] -> String
integerListToString liste seperator = integerListToString_ liste seperator False

integerListToString_ :: [Integer] -> [Char] -> Bool -> String
integerListToString_ [] _ _                           = []
integerListToString_ (x:xs) seperator addSepertator     | addSepertator = seperator ++ show x ++ (integerListToString_ xs seperator True)
                                                        | otherwise = show x ++ (integerListToString_ xs seperator True)

-- Konvertiert eine Liste aus Charaktären in eine Integer-Liste
charListToAsciiIntegerList :: [Char] -> [Integer]
charListToAsciiIntegerList list = map (\z -> toInteger $ ord z) list

-- Konvertiert eine Liste von Strings zu einer Liste von Integer'n
stringListToIntegerList :: [String] -> [Integer]
stringListToIntegerList list = map (\s -> read s :: Integer) list

-- mapt eine Liste aus Integern zu einem String
integerListToCharString :: [Integer] -> String
integerListToCharString list = map integerToChar list

-- Konvertiert ein Integer zu einem Character (hierbei ist einer Integer der Ascii-Wert dieses Characters)
integerToChar :: Integer -> Char
integerToChar n = chr (fromIntegral n)