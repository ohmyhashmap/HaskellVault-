module Mahasiswa where

-- Tipe Data untuk Mahasiswa
data Mahasiswa = Mahasiswa
  { nama    :: String  -- Nama Mahasiswa
  , lulus   :: Bool    -- Status Lulus (True = Lulus, False = Tidak Lulus)
  , nilaiMatematika :: Double -- Nilai Matematika Mahasiswa
  } deriving Show

-- Fungsi untuk Menampilkan Detail Mahasiswa
getDetailMahasiswa :: Mahasiswa -> String
getDetailMahasiswa mahasiswa = 
  "Nama: " ++ nama mahasiswa ++ "\n" ++
  "Lulus: " ++ show (lulus mahasiswa) ++ "\n" ++
  "Nilai Matematika: " ++ show (nilaiMatematika mahasiswa)

-- Fungsi untuk Mengecek Status Kelulusan Berdasarkan Nilai
cekKelulusan :: Mahasiswa -> String
cekKelulusan mahasiswa
  | nilaiMatematika mahasiswa >= 60 = "Mahasiswa Lulus"
  | otherwise = "Mahasiswa Tidak Lulus"

-- Fungsi untuk Mengubah Nilai Matematika Mahasiswa
ubahNilaiMatematika :: Double -> Mahasiswa -> Mahasiswa
ubahNilaiMatematika nilaiBaru mahasiswa = mahasiswa { nilaiMatematika = nilaiBaru }

-- Fungsi untuk Mengubah Status Kelulusan Mahasiswa
ubahStatusLulus :: Bool -> Mahasiswa -> Mahasiswa
ubahStatusLulus statusBaru mahasiswa = mahasiswa { lulus = statusBaru }

-- Contoh Program Utama
main :: IO ()
main = do
  -- Membuat Data Mahasiswa
  let mahasiswa1 = Mahasiswa "Budi" False 75.0
  let mahasiswa2 = Mahasiswa "Siti" False 55.0
  
  -- Menampilkan Detail Mahasiswa
  putStrLn "=== Detail Mahasiswa 1 ==="
  putStrLn (getDetailMahasiswa mahasiswa1)
  
  putStrLn "=== Detail Mahasiswa 2 ==="
  putStrLn (getDetailMahasiswa mahasiswa2)
  
  -- Mengecek Kelulusan
  putStrLn "\n=== Cek Kelulusan Mahasiswa 1 ==="
  putStrLn (cekKelulusan mahasiswa1)
  
  putStrLn "\n=== Cek Kelulusan Mahasiswa 2 ==="
  putStrLn (cekKelulusan mahasiswa2)
  
  -- Mengubah Nilai Matematika Mahasiswa 2
  let mahasiswa2Baru = ubahNilaiMatematika 65.0 mahasiswa2
  putStrLn "\n=== Setelah Mengubah Nilai Matematika Mahasiswa 2 ==="
  putStrLn (getDetailMahasiswa mahasiswa2Baru)
  
  -- Mengubah Status Lulus Mahasiswa 1
  let mahasiswa1Baru = ubahStatusLulus True mahasiswa1
  putStrLn "\n=== Setelah Mengubah Status Lulus Mahasiswa 1 ==="
  putStrLn (getDetailMahasiswa mahasiswa1Baru)
