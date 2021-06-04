import Data.Time.Clock

erweiteter_euclid :: Integer -> Integer -> (Integer, Integer, Integer)
erweiteter_euclid a b   | b == 0    = (a, 1, 0) 
                        | otherwise = (x, z, y - div a b * z)
                            where
                                reku = erweiteter_euclid b $ mod a b
                                x = get_1 reku
                                y = get_2 reku
                                z = get_3 reku

get_1 :: (x, y, z) -> x
get_1 (x, _, _) = x

get_2 :: (x, y, z) -> y
get_2 (_, y, _) = y

get_3 :: (x, y, z) -> z
get_3 (_, _, z) = z

-- n :: Integer
-- n = 13 * 11

-- phiN :: Integer
-- phiN = 12 * 10

-- publicKey :: Integer -> Integer -> Integer -> (Integer, Integer)
-- publicKey p q e = if (e > 1 && e < phiN && (get_1 (erweiteter_euclid phiN e) == 1)) -- Nachher mit ggT ersetzen, siehe unten
--                   then (e, n) 
--                   else (0, 0)
--                     where
--                         n = p * q -- Später evtl. p, q einlesen und dann N und phiN als ,,globale Variable" (=Konstanten) definieren
--                         phiN = (p - 1) * (q - 1)
--                     --     ggT = (.) get_1 erweiteter_euclid Nochmal gucken wie man das richtig kompositioniert

-- privateKey :: Integer -> Integer -> Integer -> (Integer, Integer)
-- privateKey p q e = (d, n)
--                         where
--                             n = p * q
--                             phiN = (p - 1) * (q - 1)
--                             d = get_3 (erweiteter_euclid phiN e)

--p q e -> ((e, n), (d, n), Bool) wobei (e,n) der public und (d, n) der private Key ist. Bool gibt an ob die Generierung erfolgreich war.
generateKeys :: Integer -> Integer -> Integer -> ((Integer, Integer), (Integer, Integer), Bool)
generateKeys p q e  | not (isPrime p && isPrime q) 
                        || not (e > 1 && e < phiN && teilerfremd == 1) = ((0, 0), (0, 0), False) 
                    | otherwise = ((e, n), (d, n), True)     
                        where
                            n = p * q
                            phiN = (p - 1) * (q - 1)
                            euklidRes = erweiteter_euclid phiN e
                            teilerfremd = get_1 euklidRes
                            d = get_3 euklidRes
                            isPrime = (\n -> True)

-- fakultaet :: Integer -> Integer
-- fakultaet 0 = 1
-- fakultaet n = n * fakultaet (n - 1) 

-- binoRechner :: (Integer, Integer) -> Integer
-- binoRechner (n, k) = fakultaet n `div` (fakultaet k * fakultaet (n - k))

-- binoRechnerJr :: (Integer, Integer, Integer) -> Integer
-- binoRechnerJr (nFak, k, n) = nFak `div` (fakultaet k * fakultaet (n - k))

-- binoList :: Integer -> [Integer]
-- binoList n  = [binoRechnerJr (nFak, k, n) | k <- [2..(n-1)]]
--                 where
--                     nFak = fakultaet n

-- integerToFloating :: Integer -> Float
-- integerToFloating x = read (show x) :: Float

-- isPrime :: Integer -> Bool
-- isPrime n = mod  (sum (binoList n)) n == 0

-- isTest :: Integer -> Bool
-- isTest n = not (elem True (map (\x -> mod n x == 0) [k | k <- [2.. div n 2]]))