{- komen panjang -}
{- Christian Alberto Sitohang 11s24039 -}
-- Tanda tangan tipe untuk fungsi luasSegitiga
luasSegitiga :: Double -> Double -> Double 
luasSegitiga a t =  0.5 * a * t

    -- Main program
main :: IO ()
main = do
    --  "Masukkan nilai : "
    a <- readLn :: IO Double
    t <- readLn :: IO Double

    -- Correctly applying the luasSegitiga function
    let result = luasSegitiga a t
    putStrLn $ show result