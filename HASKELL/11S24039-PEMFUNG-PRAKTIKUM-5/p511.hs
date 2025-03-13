-- Menghitung jumlah elemen dalam list
-- nElement :: [a] -> Int
nElement [] = 0  -- Basis: saat list kosong, hentikan proses
nElement (_:listSisa) = 1 + nElement listSisa  -- Rekursi dengan memotong elemen pertama list

-- Menjumlahkan semua elemen dalam list
-- sumElement :: Num a => [a] -> a
sumElement [] = 0  -- Basis: saat list kosong, hentikan proses
sumElement (element:listSisa) = element + sumElement listSisa  -- Rekursi untuk menjumlahkan elemen

-- Fungsi utama
main :: IO ()
main = do
    let lst = [1..5] 
    let totalElement = nElement lst  -- Menghitung jumlah elemen
    let jumlahElement = sumElement lst  -- Menjumlahkan elemen
    -- Num a => [a]: 
    -- Ini berarti list tersebut ([a]) berisi elemen-elemen dari tipe a, di mana a harus numerik, yaitu termasuk dalam kelas Num.
    -- Dengan kata lain, fungsi yang memiliki tipe Num a => [a] hanya akan bekerja dengan list yang elemennya adalah tipe numerik.
    putStrLn ("Total Element: " ++ show totalElement)  -- Menampilkan total elemen
    putStrLn ("Jumlah Element: " ++ show jumlahElement)  -- Menampilkan jumlah elemen