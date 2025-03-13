-- Tanda tangan tipe untuk fungsi rumusUn
rumusUn :: Int -> Int -> Int -> Int
-- Fungsi untuk menghitung suku ke-n
rumusUn a 1 d = a
rumusUn a n d = a + (n - 1) * d

-- Tanda tangan tipe untuk fungsi rumusSn
rumusSn :: Int -> Int -> Int -> Int
-- Fungsi untuk menghitung jumlah n suku pertama (rumusSn)
rumusSn a n d = (n * (a + rumusUn a n d)) `div` 2

-- Main program
main :: IO ()
main = do
    putStrLn "Masukkan nilai suku pertama: "
    x <- readLn :: IO Int
    putStrLn "Masukkan nilai urutan suku yang ingin dicari: "
    y <- readLn :: IO Int
    putStrLn "Masukkan nilai beda: "
    z <- readLn :: IO Int

    -- Correctly applying the rumusSn function
    let result = rumusUn x y z
    putStrLn $ "Jumlah dari deret aritmatika: " ++ show result
    let result1 = rumusSn x y z
    putStrLn $ "Jumlah total dari suku ke-1 ke suku ke-3 " ++ show y ++  " dari deret aritmatika dengan beda ("++ show z ++  ") : " ++ show result1