-- isValid(x) memeriksa apakah sebuah bilangan integer (x)
-- valid atau tidak.
-- Valid jika	bilangan x berada pada rentang nilai 0
-- sampai 100, bilangan lainnya dianggap tidak valid.

isValid :: Int -> Bool
isValid x = x >= 0 && x <= 100

main :: IO()
main = do

-- memanggil fungsi isValid
    putStrLn $ "Apakah -10 valid? " ++ show (isValid (-10))
    putStrLn $ "Apakah -1 valid? " ++ show (isValid (-1))
    putStrLn $ "Apakah 0 valid? " ++ show (isValid 0)
    putStrLn $ "Apakah 1 valid? " ++ show (isValid 1)
    putStrLn $ "Apakah 10 valid? " ++ show (isValid 10)
    putStrLn $ "Apakah 100 valid? " ++ show (isValid 100)
    putStrLn $ "Apakah 101 valid? " ++ show (isValid 101)
    putStrLn $ "Apakah 111 valid? " ++ show (isValid 111)