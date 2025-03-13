-- CHRISTIAN ALBERTO SITOHANG 11S24043
-- STUDI KASUS PERTAMA NILAI MiN4
-- haskell code :w
-- min2(x, y) mengambil nilai terkecil dari 2 bilangan integer
min2 :: Int -> Int -> Int
-- TODO: realisasi Haskell dis q    ini
min2 v x = 
    if v < x
        then v
        else x
    -- Fungsi min2 memeriksa apakah v lebih kecil dari x, dan mengembalikan v jika true, sebaliknya mengembalikan x.

-- min3(v, x, y) mengambil nilai terkecil dari 3 bilangan integer
min3 :: Int -> Int -> Int -> Int
-- TODO: realisasi Haskell disini
min3 v x y = min2 (min2 v x) y
    -- Fungsi min3 menggunakan min2 dua kali untuk mencari nilai terkecil dari v, x, dan y.

-- min4(v, x, y, z) mengambil nilai terkecil dari 4 bilangan integer
min4 :: Int -> Int -> Int -> Int -> Int
-- TODO: realisasi Haskell disini
min4 v x y z = min2 (min3 v x y) z
    -- Fungsi min4 menggunakan min3 dan min2 untuk menemukan nilai terkecil dari v, x, y, dan z.

-- Fungsi main untuk menjalankan program dan membaca input dari pengguna
main :: IO ()
main = do
    -- Membaca empat bilangan integer dari input
    v <- readLn :: IO Int --v <- readLn :: IO Int maksudnya adalah  

-- Pada Haskell, pernyataan <- readLn :: IO Int digunakan untuk membaca input dari pengguna, kemudian mengonversinya menjadi tipe Int.
--  Mari kita bahas lebih dalam:

-- Bagian-bagian dari pernyataan tersebut:
-- readLn :: IO Int:

-- readLn adalah fungsi yang membaca input dari standar input (biasanya dari keyboard) dan secara otomatis 
-- mengonversi input tersebut menjadi tipe data yang sesuai.
-- :: IO Int adalah anotasi tipe yang menunjukkan bahwa readLn akan menghasilkan nilai bertipe IO Int, 
-- yaitu input akan dikembalikan sebagai aksi IO yang menghasilkan nilai bertipe Int. Efek samping (seperti membaca input) 
--di Haskell dikemas dalam
--konteks IO untuk mengisolasi efek samping dari perhitungan murni.

-- Simbol <- digunakan dalam do notation untuk "mengeluarkan" nilai dari konteks IO. Ini berarti bahwa ketika kita menulis 
-- v <- readLn, Haskell membaca input, mengonversinya menjadi Int, dan kemudian mengekstrak nilai Int tersebut ke variabel v.
-- v sekarang akan memiliki tipe Int, yang bisa digunakan dalam perhitungan atau operasi berikutnya.

    x <- readLn :: IO Int
    y <- readLn :: IO Int
    z <- readLn :: IO Int
    -- Memanggil fungsi min4 dan menampilkan hasilnya
    print (min4 v x y z)