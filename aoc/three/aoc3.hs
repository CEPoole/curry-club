
import Data.Char

combine :: [Int] -> [Int] -> [Int]
combine x [] = x
combine x y =
   zipWith (+) x1 y where
     x1 = map fromBinary x

weighted :: Int -> Int -> Int
weighted i weight = if i > 0 then 1 else weight

fromBinary :: Int -> Int
fromBinary i = weighted i (-1)

toBinary :: Int -> Int
toBinary i = weighted i 0

invert :: Int -> Int
invert i = if i == 0 then 1 else 0

parseInput :: [String] -> [[Int]]
parseInput input = map format input where
  format str = map toInt str where
    toInt c = ord c-48

getInput :: FilePath -> IO [String]
getInput path = fmap lines (readFile path)

calculate :: [[Int]] -> [Int]
calculate dataset = loop dataset [] where
  loop [] result = result
  loop (h:t) runningTotal = loop t (combine h runningTotal)

toDecimal :: [Int] -> Int
toDecimal = foldl (\a b -> a * 2 + b) 0

main = do lnes <- getInput "input.txt"
          let dataset = parseInput lnes
          let mostCommon = calculate dataset
          let gamma = map toBinary mostCommon
          let epsilon = map invert gamma
          print $ toDecimal gamma * toDecimal epsilon
