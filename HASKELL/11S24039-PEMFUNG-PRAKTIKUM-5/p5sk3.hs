fibonacci :: Integer -> Integer -> Integer -> Integer
fibonacci a b 1 = a       -- Basis case: F(1) = a
fibonacci a b 2 = b          -- Basis case: F(2) = b
fibonacci a b n = fibonacci a b (n-1) + fibonacci a b (n-2)  -- Recursive case

main :: IO ()
main = do
    a <- readLn :: IO Integer
    b <- readLn :: IO Integer
    n <- readLn :: IO Integer
    let urutan = fibonacci a b n
    putStrLn $ "fibbonaci(" ++ show a ++ ", " ++ show b ++ ", " ++ show n ++") = " ++ show urutan -- salah urutan