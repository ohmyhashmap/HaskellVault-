faktorial :: Int -> Int
faktorial 0 = 1
faktorial n = n * faktorial(n-1)

main = do
    input <- readLn :: IO Int
    putStrLn $ "Faktorial (" ++ show input ++") : " ++ show (faktorial input)