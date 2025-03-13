-- Fungsi untuk menghitung pangkat dua
c :: Int -> Int
c x = x * x

-- Fungsi untuk menghitung pangkat dua dari hasil pengurangan dua bilangan
cd :: Int -> Int -> Int
cd x y = c(x - y)

-- Fungsi untuk menghitung akar dari penjumlahan pangkat dua dari hasil pengurangan
ls :: Int -> Int -> Int -> Int -> Double
ls x1 x2 y1 y2 = do
    let cd1 = cd y2 y1
    let cd2 = cd x2 x1
    let z = fromIntegral (cd1 + cd2)  -- Mengubah hasil penjumlahan ke tipe Double
    sqrt z  -- Mengembalikan akar kuadrat dari z

-- Fungsi utama
main :: IO ()
main = do 
    v <- readLn :: IO Int
    x <- readLn :: IO Int
    y <- readLn :: IO Int
    z <- readLn :: IO Int
    -- Memanggil fungsi ls dan menampilkan hasilnya
    print (ls v x y z)
