data Siswa = Siswa { nama :: String, kelas :: String, nilai :: [Int], lulus :: Bool } deriving Show

tambahNilai :: Siswa -> Int -> Siswa
tambahNilai siswa nilaiBaru = siswa { nilai = nilai siswa ++ [nilaiBaru] }

rataRata :: Siswa -> Double
rataRata siswa =
    let totalNilai = sum (nilai siswa)
        jumlahNilai = length (nilai siswa)
    in if jumlahNilai > 0
        then fromIntegral totalNilai / fromIntegral jumlahNilai
        else 0

cekLulus :: Siswa -> Siswa
cekLulus siswa = siswa { lulus = rataRata siswa >= 60 }

main :: IO ()
main = do
    -- Membuat data siswa 
    let siswa1 = Siswa { nama = "Christian", kelas = "Informatika", nilai = [70, 80, 90], lulus = False }

    -- Menambahkan nilai
    let siswa1Baru = tambahNilai siswa1 85   

    -- Menampilkan rata-rata dan status kelulusan
    putStrLn $ "Nama: " ++ nama siswa1Baru
    putStrLn $ "Kelas: " ++ kelas siswa1Baru
    putStrLn $ "Rata-rata Nilai: " ++ show (rataRata siswa1Baru)

    -- Cek status lulus
    let siswa1Final = cekLulus siswa1Baru
    putStrLn $ "Lulus: " ++ show (lulus siswa1Final)
