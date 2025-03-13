-- Nama File : HitungIndeks.hs
-- Nama Modul : HitungIndeks
-- Definisi dan Spesifikasi
-- hitungIndeks : Float -> Float -> Float -> Int
-- hitungIndeks nilaiUTS nilaiUAS nilaiTubes menerima 3 nilai (nilaiUTS, nilaiUAS, nilaiTubes)
-- bertipe Float dan mengembalikan indeks dalam bentuk bilangan bulat sesuai aturan
-- yang diberikan. Setiap indeks dilambangkan dengan angka: A = 4, B = 3, C = 2, D = 1, E = 0.

-- Realisasi
hitungIndeks :: Float -> Float -> Float -> Int
hitungIndeks nilaiUTS nilaiUAS nilaiTubes
    | nilaiUTS == 0 || nilaiUAS == 0 || nilaiTubes == 0 = 0 -- Satu nilai bernilai 0, indeks E
    | nilaiUTS < 40 || nilaiUAS < 40 = 1                  -- UTS atau UAS di bawah 40, indeks D
    | nilaiTubes < 40 && nilaiUTS >= 40 && nilaiUAS >= 40 = 2 -- Tubes di bawah 40, UTS dan UAS >= 40, indeks C
    | nilaiUTS < 75 && nilaiUAS < 75 && nilaiTubes < 75 = 2   -- Semua nilai < 75 dan tidak ada yang < 40, indeks C
    | (nilaiUTS >= 75 && nilaiUAS >= 75 && nilaiTubes < 75) ||
        (nilaiUTS >= 75 && nilaiUAS < 75 && nilaiTubes >= 75) ||
        (nilaiUTS < 75 && nilaiUAS >= 75 && nilaiTubes >= 75) = 3 -- 1 atau 2 nilai >= 75, indeks B
    | nilaiUTS >= 75 && nilaiUAS >= 75 && nilaiTubes >= 75 = 4 -- Semua nilai >= 75, indeks A
    | otherwise = error "Input tidak valid" -- Guard untuk error handling

-- Contoh aplikasi
-- > hitungIndeks 100 100 0
-- 0
-- > hitungIndeks 100 100 20
-- 2
-- > hitungIndeks 100 20 100
-- 1
-- Main program     
main :: IO ()
main = do
    -- Meminta input dari pengguna
    a <- readLn :: IO Float -- lama waktu managerial
    b <- readLn :: IO Float
    c <- readLn :: IO Float  -- lama waktu software enggineering
    -- Menghitung nilai maksimum dari tiga angka
    let hasil = hitungIndeks a b c 
    putStrLn $ show hasil