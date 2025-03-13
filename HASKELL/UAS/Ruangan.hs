module Ruangan where
-- Tipe Data untuk Ruangan
data Ruangan = Ruangan
  { lokasi :: String  -- Lokasi ruangan (misalnya, "Lantai 1, Gedung A")
  , panjang :: Double -- Panjang ruangan
  , lebar   :: Double -- Lebar ruangan
  , tinggi  :: Double -- Tinggi ruangan
  } deriving Show

-- Selektor Eksplisit untuk Mengakses Field Ruangan
getLokasi :: Ruangan -> String
getLokasi (Ruangan lokasi _ _ _) = lokasi

getPanjang :: Ruangan -> Double
getPanjang (Ruangan _ panjang _ _) = panjang

getLebar :: Ruangan -> Double
getLebar (Ruangan _ _ lebar _) = lebar

getTinggi :: Ruangan -> Double
getTinggi (Ruangan _ _ _ tinggi) = tinggi

-- Fungsi untuk Menghitung Volume Ruangan
hitungVolume :: Ruangan -> Double
hitungVolume ruangan = getPanjang ruangan * getLebar ruangan * getTinggi ruangan

-- Fungsi untuk Menampilkan Detail Ruangan
getDetailRuangan :: Ruangan -> String
getDetailRuangan ruangan = 
  "\nLokasi: " ++ getLokasi ruangan ++ "\n" ++
  "Panjang: " ++ show (getPanjang ruangan) ++ " meter\n" ++
  "Lebar: " ++ show (getLebar ruangan) ++ " meter\n" ++
  "Tinggi: " ++ show (getTinggi ruangan) ++ " meter\n" ++
  "Volume: " ++ show (hitungVolume ruangan) ++ " meter kubik"

-- Fungsi untuk Mengubah Lokasi Ruangan
ubahLokasi :: String -> Ruangan -> Ruangan
ubahLokasi lokasiBaru ruangan = ruangan { lokasi = lokasiBaru }

-- Fungsi untuk Mengubah Dimensi Ruangan (Panjang, Lebar, Tinggi)
ubahDimensi :: Double -> Double -> Double -> Ruangan -> Ruangan
ubahDimensi panjangBaru lebarBaru tinggiBaru ruangan = 
  ruangan { panjang = panjangBaru, lebar = lebarBaru, tinggi = tinggiBaru }

-- Fungsi untuk Memastikan Dimensi Ruangan Valid
validasiDimensi :: Double -> Double -> Double -> Bool
validasiDimensi panjang lebar tinggi = 
  panjang > 0 && lebar > 0 && tinggi > 0

-- Fungsi untuk Membandingkan Dua Ruangan Berdasarkan Volume
bandingkanVolume :: Ruangan -> Ruangan -> String
bandingkanVolume ruangan1 ruangan2
  | hitungVolume ruangan1 > hitungVolume ruangan2 = "Ruangan 1 lebih besar dari Ruangan 2"
  | hitungVolume ruangan1 < hitungVolume ruangan2 = "Ruangan 1 lebih kecil dari Ruangan 2"
  | otherwise = "Ruangan 1 dan Ruangan 2 memiliki volume yang sama"

-- Fungsi untuk Membandingkan Dua Ruangan Berdasarkan Dimensi
bandingkanDimensi :: Ruangan -> Ruangan -> String
bandingkanDimensi ruangan1 ruangan2
  | (getPanjang ruangan1, getLebar ruangan1, getTinggi ruangan1) > (getPanjang ruangan2, getLebar ruangan2, getTinggi ruangan2) = 
      "Ruangan 1 lebih besar dari Ruangan 2"
  | (getPanjang ruangan1, getLebar ruangan1, getTinggi ruangan1) < (getPanjang ruangan2, getLebar ruangan2, getTinggi ruangan2) = 
      "Ruangan 1 lebih kecil dari Ruangan 2"
  | otherwise = "Ruangan 1 dan Ruangan 2 memiliki dimensi yang sama"

-- Contoh Program Utama
main :: IO ()
main = do
  -- Membuat Ruangan
  let ruangan1 = Ruangan "Lantai 1, Gedung A" 5.0 4.0 3.0
  let ruangan2 = Ruangan "Lantai 2, Gedung B" 6.0 4.5 3.5
  
  -- -- Menampilkan Detail Ruangan
  -- putStrLn "=== Detail Ruangan 1 ==="
  -- putStrLn (getDetailRuangan ruangan1)
  
  -- putStrLn "=== Detail Ruangan 2 ==="
  -- putStrLn (getDetailRuangan ruangan2)
  
  -- -- Membandingkan Volume dan Dimensi
  -- putStrLn "\n=== Membandingkan Volume ==="
  -- putStrLn (bandingkanVolume ruangan1 ruangan2)
  
  -- putStrLn "\n=== Membandingkan Dimensi ==="
  -- putStrLn (bandingkanDimensi ruangan1 ruangan2)
  
  -- Mengubah Lokasi Ruangan 1
  let ruangan1Baru = ubahLokasi "Lantai 3, Gedung C" ruangan1
  -- putStrLn "\n=== Setelah Mengubah Lokasi Ruangan 1 ==="
  -- putStrLn (getDetailRuangan ruangan1Baru)
  
  -- Mengubah Dimensi Ruangan 2
  let ruangan2Baru = ubahDimensi 7.0 5.0 3.8 ruangan2
  -- putStrLn "\n=== Setelah Mengubah Dimensi Ruangan 2 ==="
  -- putStrLn (getDetailRuangan ruangan2Baru)
  
  -- Validasi Dimensi Ruangan Baru
  -- putStrLn "\n=== Validasi Dimensi Ruangan 1 Baru ==="
  if validasiDimensi (getPanjang ruangan1Baru) (getLebar ruangan1Baru) (getTinggi ruangan1Baru)
    then putStrLn "Dimensi Ruangan 1 valid."
    else putStrLn "Dimensi Ruangan 1 tidak valid."
