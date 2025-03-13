{- komen panjang -}
{- Christian Alberto Sitohang 11s24039 -}

-- Tanda tangan tipe untuk fungsi seleksi asumsikan m s dan p bilangan berbeda
seleksi1 :: Integer -> Integer -> Char -> Bool
seleksi1 m s p = 
    if p == 'A' 
        then m >= 2 && s >= 4 
        else if p == 'B'
            then m < 2 && s >= 4
            else if  p == 'C'
                then True
                else if p == 'D'
                    then m >= 2 && s < 4 
                    else False

-- Realisasi
seleksi2 :: Int -> Int -> Char -> Bool
seleksi2 m s p
    | p == 'A' = m >= 2 && s >= 4                     -- Pekerjaan A memerlukan managerial >= 2 tahun dan software engineer >= 4 tahun
    | p == 'B' = (m < 2 && s >= 4) || (m >= 2 && s >= 4) -- Pekerjaan B memerlukan s >= 4 tahun dan bisa managerial < 2 tahun
    | p == 'D' = m >= 2 && s < 4                      -- Pekerjaan D memerlukan managerial >= 2 tahun dan software engineer < 4 tahun
    | p == 'C' = True                                 -- Semua pelamar dapat melamar untuk pekerjaan C
    | otherwise = False   

-- Main program     
main :: IO ()
main = do
    -- Meminta input dari pengguna
    m <- readLn :: IO Integer -- lama waktu managerial
    s <- readLn :: IO Integer -- lama waktu software enggineering
    p <- readLn :: IO Char

    -- Menghitung nilai maksimum dari tiga angka
    let hasil = seleksi1 m s p
    putStrLn $ show hasil