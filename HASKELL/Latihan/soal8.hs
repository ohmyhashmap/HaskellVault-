{- komen panjang -}
{- Christian Alberto Sitohang 11s24039 -}

-- Tanda tangan tipe untuk fungsi max3 asumsikan a b dan c bilangan berbeda
max3 :: Integer -> Integer -> Integer -> Integer
max3 a b c = 
    if a >= b 
        then 
            if a >= c
                then a
            else c
    else 
            if b >= c
                then b
            else c

-- Main program
main :: IO ()
main = do
    -- Meminta input dari pengguna
    a <- readLn :: IO Integer
    b <- readLn :: IO Integer
    c <- readLn :: IO Integer

    -- Menghitung nilai maksimum dari tiga angka
    let hasil = max3 a b c
    putStrLn $ show hasil
