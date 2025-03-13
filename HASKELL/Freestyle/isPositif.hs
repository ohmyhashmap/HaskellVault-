-- isPositif(x) memeriksa apakah sebuah bilangan integer itu
-- positif atau tidak
isPositif :: Int -> Bool
isPositif x = x >= 0
main :: IO()
main = do
-- memanggil fungsi isPositif
    putStrLn $ "Apakah -10 positif? " ++ show (isPositif (-10))
    putStrLn $ "Apakah -1 positif? " ++ show (isPositif (-1))
    putStrLn $ "Apakah 0 positif? " ++ show (isPositif 0)
    putStrLn $ "Apakah 1 positif? " ++ show (isPositif 1)
    putStrLn $ "Apakah 10 positif? " ++ show (isPositif 10)
