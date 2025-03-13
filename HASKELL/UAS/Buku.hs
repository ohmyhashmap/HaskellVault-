-- Data Type for Book
data Buku = Buku {
  judul       :: String, -- Judul buku
  pengarang   :: String, -- Nama pengarang
  tahunTerbit :: Int,    -- Tahun terbit
  isbn10      :: String, -- ISBN 10 
  isbn13      :: String, -- ISBN 13
  key         :: Int     -- Caesar Cipher key
} deriving Show
--  STRING = [CHAR]

-- Constructor

-- Fungsi untuk membuat buku untuk membentuk Buku, dari judul j hh, menit mm, dan detik ss.
buatBuku :: String -> String -> Int -> String -> String -> Int -> Buku
buatBuku jj pp tt i10 i13 k = Buku { judul = jj, pengarang = pp, tahunTerbit = tt, isbn10 = i10, isbn13 = i13, key = k }

-- Selectors

-- Selektor untuk mengambil Judul Buku
getJudul :: Buku -> String        
getJudul buku = judul buku

-- Selektor untuk mengambil Pengarang Buku
getPengarang :: Buku -> String
getPengarang buku = pengarang buku

-- Selektor untuk mengambil Tahun Terbit Buku
getTahunTerbit :: Buku -> Int
getTahunTerbit buku = tahunTerbit buku

-- Selektor untuk mengambil ISBN10 Buku
getISBN10 :: Buku -> String
getISBN10 buku = isbn10 buku

-- Selektor untuk mengambil ISBN13 Bugku   
getISBN13 :: Buku -> String
getISBN13 buku = isbn13 buku

-- Selektor untuk mengambil kunci caesar Buku
getKey :: Buku -> Int
getKey buku = key buku

-- Operator Functions
-- Everything is Function
-- Konversi karakter ke angka
charToInt :: Char -> Int
charToInt '0' = 0
charToInt '1' = 1
charToInt '2' = 2
charToInt '3' = 3
charToInt '4' = 4
charToInt '5' = 5
charToInt '6' = 6
charToInt '7' = 7
charToInt '8' = 8
charToInt '9' = 9

-- Konversi angka ke karakter
intToChar :: Int -> Char
intToChar 0 = '0'
intToChar 1 = '1'
intToChar 2 = '2'
intToChar 3 = '3'
intToChar 4 = '4'
intToChar 5 = '5'
intToChar 6 = '6'
intToChar 7 = '7'
intToChar 8 = '8'
intToChar 9 = '9'

-- Fungsi untuk menghitung panjang ISBN
hitungPanjang :: String -> Int
hitungPanjang [] = 0
hitungPanjang (_:xs) = 1 + hitungPanjang xs

-- Fungsi untuk mengkonversi karakter ISBN menjadi angka
konversiAngkaISBN10 :: Char -> Int
konversiAngkaISBN10 'X' = 10
konversiAngkaISBN10 c = charToInt c

-- Fungsi untuk mengkonversi karakter ISBN menjadi angka
konversiAngkaISBN13 :: Char -> Int
konversiAngkaISBN13 c = charToInt c 

-- Fungsi Caesar Cipher untuk ISBN
caesarCipherISBN :: String -> Int -> String
caesarCipherISBN [] _ = []
caesarCipherISBN (x:xs) key
  | x == 'X'  = 'X' : caesarCipherISBN xs key -- Tetap X untuk ISBN-10
  | x >= '0' && x <= '9' = 
      intToChar ((charToInt x + key) `mod` 10) : caesarCipherISBN xs key
  | otherwise = x : caesarCipherISBN xs key -- Abaikan karakter lain (misal, '-')

-- Fungsi Caesar Cipher untuk Dekripsi ISBN
dekripsiCaesarCipherISBN :: String -> Int -> String
dekripsiCaesarCipherISBN [] _ = []
dekripsiCaesarCipherISBN (x:xs) key -- 12
  | x == 'X'  = 'X' : dekripsiCaesarCipherISBN xs key -- Tetap X untuk ISBN-10
  | x >= '0' && x <= '9' = 
      intToChar ((charToInt x - key + 10) `mod` 10) : dekripsiCaesarCipherISBN xs key
  | otherwise = x : dekripsiCaesarCipherISBN xs key -- Abaikan karakter lain (misal, '-')

-- Fungsi untuk mengenkripsi ISBN pada tipe data Buku
enkripsiISBNBuku :: Buku -> Buku
enkripsiISBNBuku buku = buku {
    isbn10 = caesarCipherISBN (getISBN10 buku) (key buku),
    isbn13 = caesarCipherISBN (getISBN13 buku) (key buku)
  }

dekripsiISBNBuku :: Buku -> Buku
dekripsiISBNBuku bukuEnkripsi = bukuEnkripsi {
    isbn10 = dekripsiCaesarCipherISBN (getISBN10 bukuEnkripsi) (getKey bukuEnkripsi),
    isbn13 = dekripsiCaesarCipherISBN (getISBN13 bukuEnkripsi) (getKey bukuEnkripsi)
  }

-- Fungsi pembantu untuk mengambil angka dari ISBN-10
ambilAngkaISBN10 :: String -> [Int]
ambilAngkaISBN10 [] = []
ambilAngkaISBN10 (x:xs) = konversiAngkaISBN10 x : ambilAngkaISBN10 xs

-- Fungsi pembantu untuk mengambil angka dari ISBN-13
ambilAngkaISBN13 :: String -> [Int]
ambilAngkaISBN13 [] = []
ambilAngkaISBN13 (x:xs) = konversiAngkaISBN13 x : ambilAngkaISBN13 xs

-- Fungsi untuk menghitung total checksum ISBN-10
hitungTotalISBN10 :: [Int] -> Int -> Int
hitungTotalISBN10 [] _ = 0
hitungTotalISBN10 (x:xs) n = x * n + hitungTotalISBN10 xs (n + 1)

-- Fungsi untuk menghitung total checksum ISBN-13
hitungTotalISBN13 :: [Int] -> Int -> Int
hitungTotalISBN13 [] _ = 0
hitungTotalISBN13 (x:xs) posisi
  | posisi `mod` 2 == 1 = x * 1 + hitungTotalISBN13 xs (posisi + 1)  -- Posisi ganjil menggunakan bobot 1
  | otherwise           = x * 3 + hitungTotalISBN13 xs (posisi + 1)  -- Posisi genap menggunakan bobot 3

-- Fungsi untuk menghitung checksum ISBN-10
hitungcekISBN10 :: String -> Int
hitungcekISBN10 isbn = hitungTotalISBN10 (ambilAngkaISBN10 (bersihkanISBN isbn)) 1 `mod` 11

-- Fungsi untuk menghitung checksum ISBN-13
hitungcekISBN13 :: String -> Int
hitungcekISBN13 isbn = hitungTotalISBN13 (ambilAngkaISBN13 (bersihkanISBN isbn)) 1 `mod` 10

bersihkanISBN :: String -> String
bersihkanISBN [] = []
bersihkanISBN (x:xs)
  | x == '-'  = bersihkanISBN xs -- Abaikan karakter '-'
  | otherwise = x : bersihkanISBN xs

-- Predicate (operator relasional)

-- Fungsi untuk menghitung panjang ISBN10
panjangValidISBN10 :: String -> Bool
panjangValidISBN10 isbn = hitungPanjang (bersihkanISBN isbn) == 10

-- Fungsi untuk menghitung panjang ISBN13
panjangValidISBN13 :: String -> Bool
panjangValidISBN13 isbn = hitungPanjang (bersihkanISBN isbn) == 13

-- Fungsi untuk mengecek validitas karakter ISBN
karakterValid :: String -> Bool
karakterValid [] = True
karakterValid (x:xs)
  | x == '-' = karakterValid xs
  | x == 'X' = karakterValid xs
  | x >= '0' && x <= '9' = karakterValid xs
  | otherwise = False

-- Implementasi validasi ISBN-10
isValidISBN10 :: String -> Bool
isValidISBN10 isbn = panjangValidISBN10 isbn && karakterValid isbn && hitungcekISBN10 isbn == 0

-- Implementasi validasi ISBN-13
isValidISBN13 :: String -> Bool
isValidISBN13 isbn = panjangValidISBN13 isbn && karakterValid isbn && hitungcekISBN13 isbn == 0

-- Fungsi untuk validasi ISBN10 dan ISBN13 pada suatu buku
validasiISBNBuku :: Buku -> String    
validasiISBNBuku buku
  | not (panjangValidISBN10 (getISBN10 buku)) = "ISBN10: Panjang tidak valid"
  | not (karakterValid (getISBN10 buku)) = "ISBN10: Karakter tidak valid"
  | hitungcekISBN10 (getISBN10 buku) /= 0 = "ISBN10: Checksum tidak valid"
  | not (panjangValidISBN13 (getISBN13 buku)) = "ISBN13: Panjang tidak valid"
  | not (karakterValid (getISBN13 buku)) = "ISBN13: Karakter tidak valid"
  | hitungcekISBN13 (getISBN13 buku) /= 0 = "ISBN13: Checksum tidak valid"
  | otherwise = "ISBN10 dan ISBN13: Valid"



-- LIST OF LIST daftarBukuPemrograman

tambahBuku :: Buku -> [Buku] -> [Buku]
tambahBuku buku list = buku : list

-- Fungsi untuk menyisipkan sebuah buku ke dalam daftar yang sudah terurut
konsoBuku :: (Buku -> Buku -> Bool) -> Buku -> [Buku] -> [Buku] -- Higher Order Functions
konsoBuku _ buku [] = [buku]
konsoBuku cmp buku (x:xs)
  | cmp buku x = buku : x : xs
  | otherwise  = x : konsoBuku cmp buku xs

-- Fungsi untuk mengurutkan daftar buku menggunakan insertion sort
sortBuku :: (Buku -> Buku -> Bool) -> [Buku] -> [Buku] -- Higher Order Functions
sortBuku _ [] = []
sortBuku cmp (x:xs) = konsoBuku cmp x (sortBuku cmp xs)

-- Kriteria pengurutan ASCII
compareJudul :: Buku -> Buku -> Bool
compareJudul buku1 buku2 = getJudul buku1 < getJudul buku2

comparePengarang :: Buku -> Buku -> Bool
comparePengarang buku1 buku2 = getPengarang buku1 < getPengarang buku2

compareTahunTerbit :: Buku -> Buku -> Bool
compareTahunTerbit buku1 buku2 = getTahunTerbit buku1 < getTahunTerbit buku2

-- Program utama
main :: IO ()
main = do
  -- Definisikan buku-buku lain yang diperlukan
  let buku1 = buatBuku "Pemrograman Haskell" "Christian Sitohang" 2023 "0-13-609181-4" "978-0-13-609181-3" 5
      buku2 = buatBuku "Functional" "Immanuel Lumbantobing" 2022 "0-471-95869-7" "978-0-471-95869-7" 3
      buku3 = buatBuku "Dasar-dasar Haskell" "Feny Pasaribu" 2021 "0-306-40615-2" "978-0-306-40615-7" 4
      buku10 = buatBuku "Matematika Diskrit" "John Doe" 2020 "0-672-32452-5" "978-0-672-32452-0" 2
      buku11 = buatBuku "Kalkulus I" "Jane Doe" 2021 "0-13-110362-8" "978-0-13-110362-7" 1
      buku13 = buatBuku "Geometri Analitik" "Alice Smith" 2022 "0-596-00920-8" "978-0-596-00920-5" 6
      buku21 = buatBuku "Bahasa Inggris 101" "Bob Johnson" 2020 "0-262-03384-4" "978-0-262-03384-8" 3
      buku22 = buatBuku "English Grammar" "Susan Lee" 2022 "0-201-63361-2" "978-0-201-63361-0" 4
      buku26 = buatBuku "English for Beginners" "David White" 2023 "0-385-51423-9" "978-0-385-51423-1" 2  
      daftarBukuPemrograman = [buku1,buku2,buku3]
      daftarBukuMatematika = [buku10,buku11,buku13]
      daftarBukuInggris = [buku21,buku22,buku26]
      
  -- Enkripsi buku
  let buku1Enkripsi = enkripsiISBNBuku buku1
      buku2Enkripsi = enkripsiISBNBuku buku2
      buku3Enkripsi = enkripsiISBNBuku buku3
      buku10Enkripsi = enkripsiISBNBuku buku10
  -- Dekripsi buku
  let buku1Dekripsi = dekripsiISBNBuku buku1Enkripsi 
      buku2Dekripsi = dekripsiISBNBuku buku2Enkripsi
      buku3Dekripsi = dekripsiISBNBuku buku3Enkripsi
      buku10Dekripsi = dekripsiISBNBuku buku10Enkripsi
  -- Validasi ISBN Buku
  putStrLn "\n=== VALIDASI ISBN BUKU ==="
  putStrLn $ "Buku : " ++ judul buku1 ++ " - " ++ validasiISBNBuku buku1
  putStrLn $ "Buku : " ++ judul buku2 ++ " - " ++ validasiISBNBuku buku2
  putStrLn $ "Buku : " ++ judul buku3 ++ " - " ++ validasiISBNBuku buku3
  putStrLn $ "Buku : " ++ judul buku10 ++ " - " ++ validasiISBNBuku buku10
  putStrLn $ "Buku : " ++ judul buku11 ++ " - " ++ validasiISBNBuku buku11
  putStrLn $ "Buku : " ++ judul buku13 ++ " - " ++ validasiISBNBuku buku13
  putStrLn $ "Buku : " ++ judul buku21 ++ " - " ++ validasiISBNBuku buku21
  putStrLn $ "Buku : " ++ judul buku22 ++ " - " ++ validasiISBNBuku buku22
  putStrLn $ "Buku : " ++ judul buku26 ++ " - " ++ validasiISBNBuku buku26
  putStrLn "" 
  print (hitungcekISBN10 (getISBN10 buku10))
  print (hitungcekISBN10 (getISBN10 buku1)) -- 0*1+6*2+7*3+2*4+3*5+2*6+4*7+5*8+2*9+5*10
  -- Tampilkan buku sebelum enkripsi
  putStrLn "=== DAFTAR BUKU (SEBELUM ENKRIPSI) ==="
  print buku1
  print buku2
  print buku3
  -- mapM_ print daftarBukuPemrograman 

  -- Tampilkan buku setelah enkripsi
  putStrLn "\n=== DAFTAR BUKU (SETELAH ENKRIPSI) ==="
  print buku1Enkripsi
  print buku2Enkripsi
  print buku3Enkripsi
  -- let daftarBukuPemrogramanTerEnkripsi = daftarBukuPemrograman
  -- mapM_ print daftarBukuPemrogramanEnkripsi
  
  -- Tampilkan buku setelah dekripsi
  putStrLn "\n=== DAFTAR BUKU (SETELAH DEKRIPSI) ==="
  print buku1Dekripsi
  print buku2Dekripsi
  print buku3Dekripsi
  -- let daftarBukuPemrogramanDekripsi = sortBuku TerDekripsi
  -- mapM_ print daftarBukuPemrogramanDekripsi

  putStrLn "\n=== DAFTAR BUKU (SEBELUM SORTING) ==="
  mapM_ print daftarBukuPemrograman

  putStrLn "\n=== DAFTAR BUKU (SORTING BERDASARKAN JUDUL) ==="
  let sortedByJudul = sortBuku compareJudul daftarBukuPemrograman
  mapM_ print sortedByJudul

  putStrLn "\n=== DAFTAR BUKU (SORTING BERDASARKAN PENGARANG) ==="
  let sortedByPengarang = sortBuku comparePengarang daftarBukuPemrograman
  mapM_ print sortedByPengarang

  putStrLn "\n=== DAFTAR BUKU (SORTING BERDASARKAN TAHUN TERBIT) ==="
  let sortedByTahun = sortBuku compareTahunTerbit daftarBukuPemrograman
  mapM_ print sortedByTahun