-- Fungsi untuk menghitung gaji lembur
hitungGajiLembur :: Char -> Int -> Int
hitungGajiLembur golongan jamKerja
    | golongan == 'A' && jamKerja >= 1 && jamKerja <= 5 = jamKerja * 20000
    | golongan == 'A' && jamKerja > 6  = jamKerja * 25000
    | golongan == 'B' && jamKerja >= 1 && jamKerja <= 5 = jamKerja * 15000
    | golongan == 'B' && jamKerja > 6  = jamKerja * 20000
    | otherwise = 0  -- Jika tidak memenuhi kondisi, gaji lembur = 0 (misal invalid input)

-- Program utama
main :: IO ()
main = do
    putStrLn "Masukkan golongan (A/B):"
    golongan <- getLine
    putStrLn "Masukkan jumlah jam lembur:"
    jamKerja <- readLn :: IO Int
    let gajiLembur = hitungGajiLembur (head golongan) jamKerja
    putStrLn $ "Gaji lembur anda: Rp " ++ show gajiLembur
