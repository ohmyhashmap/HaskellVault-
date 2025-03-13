-- genap [1, 2, 3, 4, 5, 6] -- harus mengembalikan [2, 4, 6]

genap :: [Int] -> [Int]
genap [] = []  -- Kasus dasar: jika list kosong, hasilnya juga kosong
genap (x:xs)
    | x `mod` 2 == 0 = x : genap xs  -- Jika x genap (x mod 2 == 0), masukkan x ke hasil dan rekursi dengan xs
    | otherwise      = genap xs      -- Jika x ganjil, lewati x dan rekursi dengan xs

main :: IO ()
main = do  
    let myList = [1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 90]
    let evenNumbers = genap myList
    putStrLn $ "Bilangan genap dari list " ++ show myList ++ " adalah: " ++ show evenNumbers