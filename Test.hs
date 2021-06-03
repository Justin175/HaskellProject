import System.IO
import Utils

--digitToInt --> machts beispielsweise 'F' --> 15 oder '9' --> 9, aber NICT 'L' --> ?

getFileNameFromPath :: [Char] -> [Char]
getFileNameFromPath []      = []
getFileNameFromPath (x:xs)  = (getFileNameFromPath xs) ++ [x]