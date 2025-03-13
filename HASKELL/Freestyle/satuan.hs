-- Fungsi untuk mengkonversi meter ke kilometer
meterKeKilometer :: Double -> Double
meterKeKilometer meter = meter / 1000

-- Fungsi untuk mengkonversi meter ke centimeter
meterKeCentimeter :: Double -> Double
meterKeCentimeter meter = meter * 100

-- Fungsi untuk mengkonversi meter ke milimeter
meterKeMilimeter :: Double -> Double
meterKeMilimeter meter = meter * 1000

-- Fungsi untuk mengkonversi kilogram ke gram
kilogramKeGram :: Double -> Double
kilogramKeGram kilogram = kilogram * 1000

-- Fungsi untuk mengkonversi kilogram ke ton
kilogramKeTon :: Double -> Double
kilogramKeTon kilogram = kilogram / 1000

-- Fungsi utama
main :: IO ()
main = do
    putStrLn "Konversi Satuan:"
    
    -- Konversi meter
    putStrLn "Masukkan panjang dalam meter:"
    meterInput <- readLn :: IO Double
    putStrLn $ show meterInput ++ " meter = " ++ show (meterKeKilometer meterInput) ++ " kilometer"
    putStrLn $ show meterInput ++ " meter = " ++ show (meterKeCentimeter meterInput) ++ " centimeter"
    putStrLn $ show meterInput ++ " meter = " ++ show (meterKeMilimeter meterInput) ++ " milimeter"

    -- Konversi kilogram
    putStrLn "Masukkan berat dalam kilogram:"
    kilogramInput <- readLn :: IO Double
    putStrLn $ show kilogramInput ++ " kilogram = " ++ show (kilogramKeGram kilogramInput) ++ " gram"
    putStrLn $ show kilogramInput ++ " kilogram = " ++ show (kilogramKeTon kilogramInput) ++ " ton"
