-- faktorial :: Int -> Int -- Deklarasi tipe: menerima sebuah Int, mengembalikan sebuah Int
faktorial 0 = 1           -- Basis: faktorial dari 0 adalah 1
faktorial n = n * faktorial (n - 1)  -- Rekursif: n * faktorial dari (n-1)

main :: IO()
main = do
    input <- getLine -- Meminta dan Membaca input dari pengguna  
    let n = read input :: Int  -- Mengonversi input menjadi Int
    putStrLn $ "faktorial(" ++ show n ++ ") = " ++ show (faktorial n)