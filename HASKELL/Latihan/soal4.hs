{- komen panjang -}
{- Christian Alberto Sitohang 11s24039 -}
-- Tanda tangan tipe untuk fungsi LuasLingkaran
luasLingkaran :: Float -> Float -- bisa diganti dengan Double
luasLingkaran r =  3.1415 * r * r

-- Main program
main :: IO ()
main = do
    --  "Masukkan nilai r : "
    r <- readLn :: IO Float {- bisa diganti dengan Double untuk hasil tertentu l,
coba denan input 3.5 Float berbeda dengan Double pada hasil (output)-}

    -- Correctly applying the LuasLingkaran function
    let result = luasLingkaran r
    putStrLn $ show result