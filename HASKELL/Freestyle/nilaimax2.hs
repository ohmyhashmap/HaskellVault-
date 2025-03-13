max2 :: Int -> Int -> Int
-- TODO: realisasi Haskell disini
max2 v x = 
    if v > x
        then v
        else x
        
main :: IO ()
main = do
    v <- readLn :: IO Int
    x <- readLn :: IO Int
-- memanggil fungsi wujudZatAir
    print (max2 v x)
-- Fungsi max2 memeriksa apakah v lebih besar dari x, dan mengembalikan v jika true, sebaliknya mengembalikan x.