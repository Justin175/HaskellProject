module Utils
where 
import Data.Char

-- Wandelt die Zeit im UTC-Format als String in eine Zahl um, indem Zeichen ausgelassen werden, die keine Ziffer sind:
-- 2021-06-03 23:32:10.6951018 UTC -> 202106032332106951018
-- RETURN: Zahl die sich aus den Zahlen im UTC-String zusammensetzt
utcToInteger :: [Char] -> Integer 
utcToInteger utc = read (filter istZiffer utc) :: Integer
                    where
                        istZiffer = (\c -> ord c >= ord '0' && ord c <= ord '9')

-- Erzeugt eine Zufallszahl ausgehend von einem Startwert (=Seed) und einem Tupel, 
-- welches dem erlaubten geschlossenen Intervall entspricht, in dem die Zufallszahl liegen darf
-- Der erste Parameter ist der Seed
-- Der zweite Parameter ist das geschlossene Intervall als Tupel
-- RETURN: Zufällige Zahl im Intervall (lowerBound, upperBound)
rng :: Integer -> (Integer, Integer) -> Integer
rng seed (lowerBound, upperBound) = mod (seed + 283647013) (upperBound - lowerBound + 1) + lowerBound

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

-- Konvertiert eine Liste von Strings zu Zahlen
stringListToIntegerList :: [String] -> [Integer]
stringListToIntegerList list = map (\s -> read s :: Integer) list

-- split :: Char -> String -> [String]
-- split delimiter toSplit = [x | let index <- ]
--     where
--         laenge = length toSplit
--         founds = 0 : findAll delimiter toSplit ++ [laenge]

findAll :: (Eq a) => a -> [a] -> [Int]
findAll toFind list = findAll_ 0 toFind list

findAll_:: (Eq a) => Int -> a -> [a] -> [Int]
findAll_ currentIndex toFind [] = []
findAll_ currentIndex toFind (x:xs)
    | x == toFind   = currentIndex : (findAll_ (currentIndex + 1) toFind xs) 
    | otherwise     = findAll_ (currentIndex + 1) toFind xs

findNext :: (Eq a) => Int -> a -> [a] -> Int
findNext from toFind liste = findNext_ 0 from toFind liste

findNext_ :: (Eq a) => Int -> Int -> a -> [a] -> Int
findNext_ currentIndex from toFind [] = -1
findNext_ currentIndex from toFind (x:xs)
    | currentIndex < from || (not $ toFind == x) = findNext_ (currentIndex + 1) from toFind xs
    | otherwise = currentIndex

findNextMax :: (Eq a) => Int -> a -> [a] -> Int
findNextMax from toFind liste = findNextMax_ 0 from toFind liste

findNextMax_ :: (Eq a) => Int -> Int -> a -> [a] -> Int
findNextMax_ currentIndex from toFind [] = currentIndex
findNextMax_ currentIndex from toFind (x:xs)
    | currentIndex < from || (not $ toFind == x) = findNextMax_ (currentIndex + 1) from toFind xs
    | otherwise = currentIndex

-- Nimmt einen Bestimmten Teil einer Liste und gibt diese Zurück
takePart :: Int -> Int -> [a] -> [a]
takePart from to list = takePart_ 0 from to list

takePart_ :: Int -> Int -> Int -> [a] -> [a]
takePart_ currentIndex from to [] = []
takePart_ currentIndex from to (x:xs) 
    | currentIndex < from || currentIndex > to = takePart_ (currentIndex + 1) from to xs
    | otherwise = x : takePart_ (currentIndex + 1) from to xs

getPublicKeyFromList :: [Integer] -> (Integer, Integer)
getPublicKeyFromList [p, n] = (p, n)
getPublicKeyFromList [p, _, n] = (p, n)

getPrivateKeyFromList :: [Integer] -> (Integer, Integer)
getPrivateKeyFromList [p, n] = (p, n)
getPrivateKeyFromList [_, p, n] = (p, n)

integerListToCharString :: [Integer] -> String
integerListToCharString list = map integerToChar list

integerToChar :: Integer -> Char
integerToChar n = toEnum (read $ show n :: Int) :: Char
--integerToChar n = chr (fromIntegral n)

quadupleToKeyString :: (Show a, Show b, Show c) => (a, b, c, d) -> String
quadupleToKeyString (x, y, z, _) = show x ++ "\n" ++ show y ++ "\n" ++ show z