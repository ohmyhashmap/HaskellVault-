{- komen panjang -}
{- Christian Alberto Sitohang 11s24039 -}
-- Tanda tangan tipe untuk fungsi luasTrapesium
luasTrapesium :: Double -> Double -> Double -> Double
luasTrapesium t s1 s2 =  0.5 * t *(s1 + s2)

-- Main program
main :: IO ()
main = do
    --  "Masukkan nilai : "
    t <- readLn :: IO Double
    s1 <- readLn :: IO Double
    s2 <- readLn :: IO Double

    -- Correctly applying the luasTrapesium function
    let result = luasTrapesium t s1 s2
    putStrLn $ show result