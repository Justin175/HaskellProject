module Schluesselgenerierung
where

-- Enthält zweistellige Primzahlen, von denen bei der Schlüsselgenerierung zwei zufällig entnommen werden
primes = [11,13,17,19,23,29,31,37,41,43,47,53,59,61,67,71,73,79,83,89,97]

-- Führt den erweiterten Euklidischen Algorithmus auf zwei ganze Zahlen aus
-- Die ersten beiden Parameter sind zwei ganze Zahlen a und b
-- RETURN: Tripel (x, y, z) mit x = ggT(a, b), und a * y + b * z = x
erweiteterEuklid :: Integer -> Integer -> (Integer, Integer, Integer)
erweiteterEuklid a b   | b == 0    = (a, 1, 0) 
                       | otherwise = (x, z, y - div a b * z)
                            where
                                erwEukl = erweiteterEuklid b $ mod a b
                                x = get_1 erwEukl
                                y = get_2 erwEukl
                                z = get_3 erwEukl
                                
-- Die nachfolgenden 3 Funktionen get_n liefern die n.te Komponente aus einem Tripel mit n = 1, 2, 3
get_1 :: (x, y, z) -> x
get_1 (x, _, _) = x

get_2 :: (x, y, z) -> y
get_2 (_, y, _) = y

get_3 :: (x, y, z) -> z
get_3 (_, _, z) = z

-- Die nachfolgenden 4 Funktionen get_nQ liefern die n.te Komponente aus einem Quadrupel mit n = 1, 2, 3, 4
get_1Q :: (a, b, c, d) -> a
get_1Q (a, _, _, _) = a

get_2Q :: (a, b, c, d) -> b
get_2Q (_, b, _, _) = b

get_3Q :: (a, b, c, d) -> c
get_3Q (_, _, c, _) = c

get_4Q :: (a, b, c, d) -> d
get_4Q (_, _, _, d) = d


-- Erzeugt die für die Schlüssel relevanten Werte e, d, und N und gibt darüber Auskunft, ob die Generierung geklappt hat
-- Die ersten beiden Parameter sind die (Prim)Zahlen, die für die Konstruktion der Schlüssel verwendet werden sollen
-- Der dritte Parameter e ist eine Zahl, die die erste Komponente des öffentlichen Schlüssels bildet, wenn 1 < e < phi(N) und ggT(e, phi(N)) == 1 gilt
-- RETURN: Quadrupel (e, d, N, b) mit b := Schlüsselgenerierung ist erfolgreich
generateKeys :: Integer -> Integer -> Integer -> (Integer,Integer,Integer, Bool)
generateKeys p q e  | not (e > 1 && e < phiN && teilerfremd == 1) = (0, 0, 0, False) 
                    | otherwise = (e, d, n, True)     
                        where
                            n = p * q
                            phiN = (p - 1) * (q - 1)
                            euklidRes = erweiteterEuklid phiN e
                            teilerfremd = get_1 euklidRes
                            d = ensurePositiveCongruence (get_3 euklidRes) phiN

-- 
ensurePositiveCongruence :: Integer -> Integer -> Integer
ensurePositiveCongruence d phiN | d > 0 = d
                          | otherwise = ensurePositiveCongruence (d + phiN) phiN

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