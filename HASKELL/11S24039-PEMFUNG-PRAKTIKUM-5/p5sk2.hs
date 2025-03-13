-- pangkat :: Integer -> Integer -> Integer
-- TODO: realisasi Haskell disini
pangkat a 0 = 1 -- 12^0 =1
pangkat a n = a * pangkat a (n-1)

main :: IO()
main = do   
    input1 <- readLn :: IO Integer -- Meminta dan Membaca input dari pengguna  
    input2 <- readLn :: IO Integer-- Meminta dan Membaca input dari pengguna sebagai INT 2 + 2 = 4, STRING "2"+"2" = "22"
    let a = input1 
    let n = input2
    putStrLn $ "pangkat("++ show a ++", "++ show n ++") = " ++ show (pangkat a n) 