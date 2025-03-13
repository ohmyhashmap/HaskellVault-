-- mathABS(x) menentukan nilai absolute dari x.
mathABS :: Int -> Int
mathABS x
    | x > 0	= x
    | x == 0	= 0
    | otherwise = -x
    
main :: IO()
main = do
-- memanggil fungsi mathABS
    putStrLn $ "Nilai ABS dari -10 = " ++ show (mathABS (-10))
    putStrLn $ "Nilai ABS dari -1 = " ++ show (mathABS (-1))
    putStrLn $ "Nilai ABS dari 0 = " ++ show (mathABS 0)
    putStrLn $ "Nilai ABS dari 1 = " ++ show (mathABS 1)
    putStrLn $ "Nilai ABS dari 10 = " ++ show (mathABS 10)