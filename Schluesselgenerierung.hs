module Schluesselgenerierung
where

-- Enthält zweistellige Primzahlen, von denen bei der Schlüsselgenerierung zwei zufällig entnommen werden
primes = [11,13,17,19,23,29,31,37,41,43,47,53,59,61,67,71,73,79,83,89,97]

-- primesBig = [2673092556681*15^3048-2, 
--  499490918065850301921197603564081112780623690273420984342968690594064612108591217229304461006005170865294466527166368851,
--     2673092556681*15^3048-4,
--     10513733234846849736873637829838635104309714688896631127438692162131857778044158273164093838937083421380041997,
--     506283312611011343355256478253272463245968105632679003983305635125224133331073348553775052064302641255435067238306718511,
--     35201546659608842026088328007565866231962578784643756647773109869245232364730066609837018108561065242031153677,
--     14083359469338511572632447718747493405040362318205860500297736061630222431052998057250747900577940212317413063,
--     15579763548573297857414066649875054392128789371879472432457450095645164702139048181789700140949438093329334293]

-- Führt den erweiterten Euklidischen Algorithmus auf zwei ganze Zahlen aus
-- Die ersten beiden Parameter sind zwei ganze Zahlen a und b
-- RETURN: Triple (x, y, z) mit x = ggT(a, b), und a * y + b * z = x
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

generateKeys :: Integer -> Integer -> Integer -> (Integer,Integer,Integer, Bool)
generateKeys p q e  | not (e > 1 && e < phiN && teilerfremd == 1) = (0, 0, 0, False) 
                    | otherwise = (e, d, n, True)     
                        where
                            n = p * q
                            phiN = (p - 1) * (q - 1)
                            euklidRes = erweiteterEuklid phiN e
                            teilerfremd = get_1 euklidRes
                            d = replaceWithPositve (get_3 euklidRes) phiN

-- 
replaceWithPositve :: Integer -> Integer -> Integer
replaceWithPositve d phiN | d > 0 = d
                          | otherwise = replaceWithPositve (d + phiN) phiN