{- komen panjang -}
{- Christian Alberto Sitohang 11s24039 -}

-- Tanda tangan tipe untuk fungsi layang
layang :: Double -> Double -> Double
layang x y = 0.5 * x * y
-- Main program
main :: IO ()
main = do
    --  "Masukkan nilai diagonal pertama : "
    x <- readLn :: IO Double
    --  "Masukkan nilai diagonal kedua :
    y <- readLn :: IO Double

    -- Correctly applying the layang function
    let result = layang x y
    putStrLn $ show result