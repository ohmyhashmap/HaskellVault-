-- CHRISTIAN ALBERTO SITOHANG 11S24043
-- STUDI KASUS KEDUA WUJUD ZAT AIR
-- wujudZatAir(x) melakukan pengecekan wujud zat dari air, berdasarkan
-- suhu air x, dengan x (bilangan real) yang merupakan masukan ke fungsi

wujudZatAir :: Double -> String
-- TODO: realisasi Haskell disini
wujudZatAir x 
    | x < 0            = "PADAT"
    | x >=0 && x < 100 = "CAIR"
    | x > 100          = "GAS"

main :: IO () -- untuk fungsi awalan maka tidak bisa jalan jika punya fungsi diluar fungsi utama
main = do -- maka fungsi tidak bisa jalan fungsi utama harus tidak dispasi
    x <- readLn :: IO Double
-- memanggil fungsi wujudZatAir
    print (wujudZatAir x)