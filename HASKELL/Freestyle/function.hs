--CHRISTIAN ALBERTO SITOHANG 11S24039
-- fungsi untuk memeriksa apakah suatu bilangan itu genap atau ganjil
-- fungsi ini menerima 1 bilangan bulat dan mengembalikan data Boolean
isEven :: Int -> Bool
isEven x = x `mod` 2 == 0

-- fungsi untuk mengubah bilangan bulat menjadi nama hari
-- fungsi ini menerima 1 bilangan bulat dan mengembalikan data String
numToDay :: Int -> String
numToDay n = case n of
    1 -> "Senin"
    2 -> "Selasa"
    3 -> "Rabu"
    4 -> "Kamis"
    5 -> "Jumat"
    6 -> "Sabtu"
    7 -> "Minggu"
    _ -> "Tidak valid"

-- fungsi untuk mengambil karakter pada string di posisi tertentu
-- fungsi ini menerima 1 String dan 1 bilangan bulat dan mengembalikan data Char
charAt :: String -> Int -> Char
charAt str position -- 2 parameter (String, Int)
    | position >= 0 && position < length str = str !! position
    | otherwise = error "Posisi tidak valid"

-- fungsi untuk menghitung rata-rata pada daftar bilangan bulat
-- fungsi ini menerima daftar bilangan bulat dan mengembalikan data Double
average :: [Int] -> Double
average listInt
    | length listInt == 0 = 0.0 -- Mengatasi kasus daftar kosong untuk menghindari pembagian nol
    | otherwise = fromIntegral (sum listInt) / fromIntegral (length listInt)

-- fungsi untuk menghitung varians pada daftar bilangan bulat
-- fungsi ini menerima daftar bilangan bulat dan mengembalikan data Double
variance :: [Int] -> Double
variance listInt = sumSquaredDiff / fromIntegral (length listInt)
    where
    mean = average listInt
    squaredDiff = [(fromIntegral x - mean)^2 | x <- listInt]
    sumSquaredDiff = sum squaredDiff

-- fungsi untuk menghitung standar deviasi pada daftar bilangan bulat
-- fungsi ini menerima daftar bilangan bulat dan mengembalikan data Double
stdv :: [Int] -> Double
stdv listInt
    | length listInt <= 1 = 0.0 -- Mengatasi kasus daftar kosong atau daftar dengan satu elemen
    | otherwise = sqrt (variance listInt)

main :: IO()
main = do
    -- memanggil fungsi isEven
    putStrLn $ "Apakah 1 bilangan genap? " ++ show (isEven 1)
    putStrLn $ "Apakah 20 bilangan genap? " ++ show (isEven 20)

    -- memanggil fungsi numToDay
    putStrLn $ "Hari ke-3: " ++ numToDay 3
    putStrLn $ "Hari ke-8: " ++ numToDay 8

    -- memanggil fungsi charAt
    -- Catatan: Index selalu dimulai dari 0
    putStrLn $ "Index ke-4 dari string Abdullah : " ++ show (charAt "Abdullah" 4)

    -- memanggil fungsi average
    putStrLn $ "Nilai rata-rata dari list [2, 4, 4, 4, 5, 5, 7, 9] : " ++ show (average [2, 4, 4, 4, 5, 5, 7, 9])

    -- memanggil fungsi variansi
    putStrLn $ "Nilai variansi dari list [2, 4, 4, 4, 5, 5, 7, 9] : " ++ show (variance [2, 4, 4, 4, 5, 5, 7, 9])

    -- memanggil fungsi standar deviasi
    putStrLn $ "Nilai standar deviasi dari list [2, 4, 4, 4, 5, 5, 7, 9] : " ++ show (stdv [2, 4, 4, 4, 5, 5, 7, 9])

{-}

-}