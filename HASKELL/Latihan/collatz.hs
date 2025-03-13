bensin :: Int -> Int
bensin  1 = 0
bensin x
    | x `mod` 2 == 0 = 1 + bensin (x `div` 2)
    | otherwise = 1 + bensin (3*x + 1)

hitungbensin :: Int -> Int -> Int
hitungbensin x y
    | x > y = 0
    | otherwise = bensin x + hitungbensin (x+1) y 

main :: IO ()
main = do
    x <- readLn :: IO Int
    y <- readLn :: IO Int
    putStrLn $ "hitungbensin " ++ show x ++ " " ++ show y ++ " = " ++ show (hitungbensin x y)