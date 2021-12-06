
run :: [Int] -> Int
run input = loop input 0 where
  loop (x:[]) total = total
  loop (x:xs) total = loop xs (total + t) where
    t = if x < (head xs) then 1 else 0

getInput :: FilePath -> IO ()
getInput path = do contents <- readFile path
                   let input =  map read (lines contents) :: [Int]
                   print $ run input
