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

-- p q e -> ((e, n), (d, n), Bool) wobei (e,n) der public und (d, n) der private Key ist. Bool gibt an ob die Generierung erfolgreich war.
generateKeys :: Integer -> Integer -> Integer -> ((Integer, Integer), (Integer, Integer), Bool)
generateKeys p q e  | not (isPrime p && isPrime q) 
                        || not (e > 1 && e < phiN && teilerfremd e phiN) = ((0, 0), (0, 0), False) 
                    | otherwise = ((e, n), (d, n), True)     
                        where
                            n = p * q
                            phiN = (p - 1) * (q - 1)
                            euklidRes = erweiteter_euclid phiN e
                            teilerfremd = get_1 euklidRes
                            d = get_3 euklidRes
                            isPrime = (\n -> True)





