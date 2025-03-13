getListFromInput :: Int -> IO [Int]
getListFromInput 0 = return []
getListFromInput n = do
  x <- readLn :: IO Int
  xs <- getListFromInput (n - 1)
  return (x : xs)

main = do
  totalList <- readLn :: IO Int
  myList <- getListFromInput totalList
  putStrLn $ "myList = " ++ show myList