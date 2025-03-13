roundToOne :: Double -> Double
roundToOne x = fromIntegral (round (x * 10)) / 10

main :: IO()
main = do
    let nilaiInt :: Int
        nilaiInt = 50 -- untuk assign value harus ada tab boi wkw
    let nilaiInt2 :: Int
        nilaiInt2 = 2
    let nilaiDouble :: Double
        nilaiDouble = 20.0
    let nilaiDouble2 :: Double
        nilaiDouble2 = 11.0 -- sampai sini ngerti?
    -- melakukan penjumlahan
    let hasilPenjumlahan = nilaiInt + nilaiInt2
    let hasilPenjumlahan2 = nilaiDouble + nilaiDouble2
    print hasilPenjumlahan
    print hasilPenjumlahan2
    -- melakukan pengurangan
    let hasilPengurangan = nilaiInt - nilaiInt2
    let hasilPengurangan2 = nilaiDouble - nilaiDouble2
    print hasilPengurangan
    print hasilPengurangan2
    -- melakukan perkalian
    let hasilPerkalian = nilaiInt * nilaiInt2
    let hasilPerkalian2 = nilaiDouble * nilaiDouble2
    print hasilPerkalian
    print hasilPerkalian2
    -- melakukan pembagian
    let hasilPembagian = fromIntegral nilaiInt / fromIntegral nilaiInt2
    let hasilPembagian2 = nilaiDouble / nilaiDouble2
    print roundToOne (hasilPembagian)
    print hasilPembagian2
    -- mengambil hasil sisa bagi
    let hasilSisaBagi = nilaiInt `mod` nilaiInt2
    let hasilSisaBagi2 = floor(nilaiDouble) `mod` floor(nilaiDouble2)
    print hasilSisaBagi
    print hasilSisaBagi2
    -- melakukan pemangkatan
    let hasilPangkat = nilaiInt ^ nilaiInt2
    let hasilPangkat2 = nilaiDouble ** nilaiDouble2
    print hasilPangkat
    print hasilPangkat2
    -- mengambil hasil akar
    let hasilAkar = sqrt (fromIntegral nilaiInt)
    let hasilAkar2 = sqrt nilaiDouble2
    print hasilAkar
    print hasilAkar2