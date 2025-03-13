{- komen panjang -}
{- Christian Alberto Sitohang 11s24039 -}

-- Tanda tangan tipe untuk fungsi konversiSuhu
konversiSuhu :: Float -> Char -> Float
konversiSuhu t k
    | k == 'R' = (4 / 5) * t                 -- Konversi ke Reamur
    | k == 'F' = (9 / 5) * t + 32            -- Konversi ke Fahrenheit
    | k == 'K' = t + 273.15                  -- Konversi ke Kelvin
    | otherwise = error "Satuan suhu tidak valid"  -- Jika satuan tidak valid, tampilkan pesan error

-- Main program
main :: IO ()
main = do
    -- Meminta input dari pengguna
    t <- readLn :: IO Float
    k <- getLine
    -- Mengonversi suhu
    let hasil = konversiSuhu t (head k)  -- Mengambil karakter pertama dari string satuan
    putStrLn $ "Hasil konversi: " ++ show hasil