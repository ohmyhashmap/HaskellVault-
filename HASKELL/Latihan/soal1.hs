{- komen panjang -}
{- Christian Alberto Sitohang 11s24039 -}
-- a == u1 
-- b == beda
-- n == 

-- Tanda tangan tipe untuk fungsi rumusSn
rumusSn :: Integer -> Integer -> Integer -> Integer
-- Fungsi untuk menghitung jumlah n suku pertama (rumusSn)
rumusSn a n b = (n * ( 2 * a + ( n - 1 ) * b ) ) `div` 2

-- Main program 
main :: IO ()
main = do
    --  "Masukkan nilai suku pertama: "
    x <- readLn :: IO Integer
    --  "Masukkan nilai urutan suku yang ingin dicari: "
    y <- readLn :: IO Integer
    --  "Masukkan nilai beda: "
    z <- readLn :: IO Integer

    -- Correctly applying the rumusSn function
    let result = rumusSn x y z
    putStrLn $ show result