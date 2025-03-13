module Keuangan where
import Date
-- Tipe Data untuk Transaksi
data Transaksi = Transaksi 
  { tanggal  :: String  -- Tanggal Transaksi dalam format YYYY-MM-DD
  , deskripsi :: String -- Deskripsi Transaksi (misalnya, "Pemasukan" atau "Pengeluaran")
  , jumlah   :: Double  -- Jumlah Uang (positif untuk pemasukan, negatif untuk pengeluaran)
  } deriving Show

-- Tipe Data untuk Akun
data Akun = Akun 
  { namaPemilik :: String        -- Nama Pemilik Akun 
  , saldo       :: Double        -- Saldo Akun
  , riwayat     :: [Transaksi]   -- Daftar Transaksi yang telah dilakukan
  } deriving Show

-- Fungsi untuk Menambahkan Transaksi ke Akun
tambahTransaksi :: Transaksi -> Akun -> Akun
tambahTransaksi t akun = akun { riwayat = t : riwayat akun, saldo = saldo akun + jumlah t }

-- Fungsi untuk Membuat Transaksi Secara Manual
buatTransaksi :: String -> String -> Double -> Transaksi
buatTransaksi tgl desc amt = Transaksi tgl desc amt

-- Fungsi untuk Menghitung Saldo Akhir Berdasarkan Riwayat Transaksi
hitungSaldoAkhir :: Akun -> Double
hitungSaldoAkhir akun = saldo akun + sum (map jumlah (riwayat akun))

-- Fungsi untuk Menampilkan Detail Akun
getDetailAkun :: Akun -> String
getDetailAkun akun = 
  "Nama Pemilik: " ++ namaPemilik akun ++ "\n" ++
  "Saldo Awal: " ++ show (saldo akun) ++ "\n" ++
  "Riwayat Transaksi:\n" ++ unlines (map show (riwayat akun)) ++
  "Saldo Akhir: " ++ show (hitungSaldoAkhir akun)

-- Contoh Program Utama
main :: IO ()
main = do
  -- Membuat Akun
  let akun1 = Akun "John Doe" 1000.0 []
  
  -- Membuat Transaksi
  let transaksi1 = buatTransaksi "2024-11-01" "Pemasukan Gaji" 2000.0
  let transaksi2 = buatTransaksi "2024-11-05" "Pengeluaran Belanja" (-500.0)
  
  -- Menambahkan Transaksi ke Akun
  let akunDenganTransaksi1 = tambahTransaksi transaksi1 akun1
  let akunDenganTransaksi2 = tambahTransaksi transaksi2 akunDenganTransaksi1
  
  -- -- Menampilkan Detail Akun
  -- putStrLn "=== Detail Akun ==="
  -- putStrLn (getDetailAkun akunDenganTransaksi2)
