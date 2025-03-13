{- komen panjang -}
{- Christian Alberto Sitohang 11s24039 -}

-- Tanda tangan tipe untuk fungsi perskuadrat
perskuadrat :: Integer -> Integer -> Integer -> Integer -> Integer
perskuadrat a b c x = a * x * x + b * x + c
-- jadi dalam persamaan kuadrat ada nilai a b dan c
-- Main program
main :: IO ()
main = do
    --  "Masukkan nilai a : "
    a <- readLn :: IO Integer
    --  "Masukkan nilai b : "  
    b <- readLn :: IO Integer
    --  "Masukkan nilai c : "
    c <- readLn :: IO Integer
    --  "Masukkan nilai x : "
    x <- readLn :: IO Integer

    -- Correctly applying the perskuadrat function
    let result = perskuadrat a b c x
    putStrLn $ show result