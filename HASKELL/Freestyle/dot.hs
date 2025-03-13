main :: IO ()
main = do
    putStrLn "Masukkan suhu dalam derajat Celcius:"
    input <- getLine
    let suhu = read input :: Float --melihat input sebagai bilangan desimal
    putStrLn (kategoriSuhu suhu)

kategoriSuhu :: Float -> String
kategoriSuhu suhu
    | suhu <= 0  = "Dingin beku"
    | suhu <= 15 = "Dingin"
    | suhu <= 25 = "Sejuk"
    | suhu <= 35 = "Hangat"
    | suhu <= 45 = "Panas"
    | otherwise  = "Panas ekstrem!"