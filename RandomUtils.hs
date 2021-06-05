module RandomUtils
where 
import Data.Char

-- map (^10000) [20..250] hat ca 4min gedauert -> Das sind gerade mal 40 Wörter

-- Wandelt die Zeit im UTC-Format als String in eine Zahl um, indem Zeichen ausgelassen werden, die keine Ziffer sind:
-- 2021-06-03 23:32:10.6951018 UTC -> 202106032332106951018
utcToInteger :: [Char] -> Integer -- lines seperatiert einen String bei newline
utcToInteger utc = read (filter istZiffer utc) :: Integer
                    where
                        istZiffer = (\c -> ord c >= ord '0' && ord c <= ord '9')

-- Erzeugt eine Zufallszahl ausgehend von einem Startwert (=Seed) und einem Tupel, welches der erlaubten Range der Zufallszahl entspricht.
-- rng seed (2, 10) liefert eine zufällige Zahl x mit 2 <= x <= 10
rng :: Integer -> (Integer, Integer) -> Integer
rng seed (lowerBound, upperBound) = mod (seed - 283647013) (upperBound - lowerBound + 1) + lowerBound

 