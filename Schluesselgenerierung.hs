erweiteter_euclid :: Integer -> Integer -> (Integer, Integer, Integer)
erweiteter_euclid a b   | b == 0    = (a, 1, 0) 
                        | otherwise = (x, z, y - div a b * z)
                            where
                                reku = erweiteter_euclid b $ mod a b
                                x = get_1 reku
                                y = get_2 reku
                                z = get_3 reku

get_1 :: (Integer, Integer, Integer) -> Integer
get_1 (x, _, _) = x

get_2 :: (Integer, Integer, Integer) -> Integer
get_2 (_, y, _) = y

get_3 :: (Integer, Integer, Integer) -> Integer
get_3 (_, _, z) = z

-- n :: Integer
-- n = 13 * 11

-- phiN :: Integer
-- phiN = 12 * 10

publicKey :: Integer -> Integer -> Integer -> (Integer, Integer)
publicKey p q e = if (e > 1 && e < phiN && (get_1 (erweiteter_euclid phiN e) == 1)) -- Nachher mit ggT ersetzen, siehe unten
                  then (e, n) 
                  else (0, 0)
                    where
                        n = p * q -- Später evtl. p, q einlesen und dann N und phiN als ,,globale Variable" (=Konstanten) definieren
                        phiN = (p - 1) * (q - 1)
                    --     ggT = (.) get_1 erweiteter_euclid Nochmal gucken wie man das richtig kompositioniert

privateKey :: Integer -> Integer -> Integer -> (Integer, Integer)
privateKey p q e = (d, n)
                        where
                            n = p * q
                            phiN = (p - 1) * (q - 1)
                            d = get_3 (erweiteter_euclid phiN e)

