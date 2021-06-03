import System.IO
import Utils

--29.04

--digitToInt --> machts beispielsweise 'F' --> 15 oder '9' --> 9, aber NICT 'L' --> ?

main = do
    withFile "TestContent.txt" ReadMode (\handle -> do
            contents <- hGetContents handle
            putStrLn $ integerListToString (charListToAsciiIntegerList contents) " "
        )