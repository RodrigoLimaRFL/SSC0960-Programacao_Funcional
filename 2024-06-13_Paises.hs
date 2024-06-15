import System.IO
import Control.Exception (evaluate)
import Control.DeepSeq (force)
import Data.List
import Data.Ord


-- indexes of the columns in the csv file
country = 0
confirmed = 1
deaths = 2
recovery = 3
active = 4


-- splits a string by commas
parseCSVLine :: String -> [String]
parseCSVLine [] = []
parseCSVLine (',':cs) = "" : parseCSVLine cs
parseCSVLine cs = let (str, rest) = break (==',') cs
                  in str : parseCSVLine (drop 1 rest)


-- returns the sum of the values of the nth column of a list of lists
sumValue :: [[String]] -> Int -> Int
sumValue [] _ = 0
sumValue (x:xs) n = read (x !! n) + sumValue xs n


-- returns the sum of the values of the first x lists of a list of lists
sumOfXValues :: [[String]] -> Int -> Int -> Int
sumOfXValues ws x = sumValue (take x ws)


-- returns the list of lists with the values of the given column or equal to n
listGreaterOrEqualThan :: [[String]] -> Int -> Int -> [[String]]
listGreaterOrEqualThan [] _ _ = []
listGreaterOrEqualThan (x:xs) n column
    | read (x !! column) >= n = x : listGreaterOrEqualThan xs n column
    | otherwise = listGreaterOrEqualThan xs n column


-- returns n lists with the greatest value of the given column
listGreatestValue :: [[String]] -> Int -> Int -> [[String]]
listGreatestValue ws n column = take n (sortBy (comparing (Down . (\x -> read (x !! column) :: Int))) ws)


-- returns n lists with the smallest value of the given column
listSmallestValue :: [[String]] -> Int -> Int -> [[String]]
listSmallestValue ws n column = take n (sortBy (comparing (\x -> read (x !! column) :: Int)) ws)


-- returns the sum of active of all the countries with confirmed greater or equal to n
firstCase :: [[String]] -> Int -> Int
firstCase ws n = sumValue (listGreaterOrEqualThan ws n confirmed) active

-- between the x countries with the greatest active, returns the sum of deaths of the y countries with the lowest confirmed
secondCase :: [[String]] -> Int -> Int -> Int
secondCase ws x y = sumOfXValues (listSmallestValue (listGreatestValue ws x active) y confirmed) y deaths


-- returns the x countries with the greatest confirmed in alphabetical order
thirdCase :: [[String]] -> Int -> [String]
thirdCase ws x =
    let
        greatestConfirmed = listGreatestValue ws x confirmed
    in map head (sortBy (comparing head) greatestConfirmed)


printLines :: [String] -> IO ()
printLines strs = putStrLn (intercalate "\n" strs)


main :: IO ()
main = do
    h <- openFile "dados.csv" ReadMode
    c <- hGetContents h
    evaluate $ force c
    hClose h

    let ls = lines c
    let ws = map parseCSVLine ls

    command <- getLine
    let inputs = words command
    let [n1, n2, n3, n4] = map read inputs :: [Int]

    print $ firstCase ws n1
    print $ secondCase ws n2 n3
    printLines $ thirdCase ws n4
