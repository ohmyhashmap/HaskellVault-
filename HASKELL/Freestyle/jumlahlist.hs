-- jumlahList [1, 2, 3, 4] -- harus mengembalikan 10

jumlahList :: [Int] -> Int
jumlahList [] = 0  -- Kasus dasar: jika list kosong, hasilnya juga kosong
jumlahList (x : xs) = x + jumlahList(xs)

main :: IO ()
main = do
    let myList = [1, 2, 3, 4, 5, 6, 7, 8, 9, 10]
    let evenNumbers = jumlahList myList
    putStrLn $ "Bilangan jumlahList dari list " ++ show myList ++ " adalah: " ++ show evenNumbers
