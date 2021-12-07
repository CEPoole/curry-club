
import Data.Char

parseInput :: [String] -> [(String, Int)]
parseInput input = map format input where
  format str =
    (
      take (length(str) - 2) str,
      ord (last str) - 48
    )


getInput :: FilePath -> IO [String]
getInput path = fmap lines (readFile path)


run :: [(String, Int)] -> Int
run dataset = loop dataset 0 0 where
  loop [] x y = x * y
  loop (h:t) x y = case h of
    ("forward", d) -> loop t (x+d) y
    ("down", d) -> loop t x (y-d)
    ("up", d) -> loop t x (y+d)


main = do lnes <- getInput "input.txt"
          let dataset = parseInput lnes
          print $ run dataset
