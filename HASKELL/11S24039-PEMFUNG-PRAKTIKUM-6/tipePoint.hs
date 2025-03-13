data Point = Point { x :: Int, y :: Int } deriving Show     

-- konstruktor
makePoint :: Int -> Int -> Point
makePoint x y = Point{x = x, y = y }  --tipe data p
-- untuk suatu titik p memiliki = x , y = y} 

-- selektor
absis :: Point -> Int -- absis (x) sebagai nilai x suatu titik atau Point di koordinat kartesian
absis point = x point

ordinat :: Point -> Int -- sebagai nilai y suatu titik atau Point di koordinat kartesian
ordinat point = y point

isOrigin :: Point -> Bool --fungsi ini memeriksa suatu point berada di titik x = 0, y = 0
isOrigin p = absis p == 0 && ordinat p == 0

kuadran :: Point -> String
kuadran p
    | absis p > 0 && ordinat p > 0 = "1"
    | absis p > 0 && ordinat p < 0 = "2"
    | absis p < 0 && ordinat p > 0 = "3"
    | absis p < 0 && ordinat p < 0 = "4"
    | otherwise                    = "Lainnya"

    -- > isOnSbY(p) adalah fungsi untuk melakukan pengecekan apakah titik p berada pada sumbu Y.
isOnSbY :: Point -> Bool
-- TODO: realisasi Haskell disini
isOnSbY p = absis p == 0 
    
    -- > isEqual(p1, p2) adalah fungsi untuk melakukan pengecekan apakah titik p1 sama dengan titik p2.
isEqual :: Point -> Point -> Bool
-- TODO: realisasi Haskell disini
isEqual p1 p2 = absis p1 == absis p2 && ordinat p1 == ordinat p2
    -- Realisasi Operator

    -- > translasiSbY(p, n) adalah fungsi untuk mengembalikan sebuah titik hasil translasi titik p, searah sumbu Y, sejauh n.
translasiSbY :: Point -> Int -> Point
-- TODO: realisasi Haskell disini
translasiSbY p n = makePoint (absis p) (ordinat p + n)

    -- > jarak(p1, p2) adalah fungsi untuk mengembalikan sebuah nilai real, merupakan jarak antara titik p1 dan p2
jarak :: Point -> Point -> Double
-- TODO: realisasi Haskell disini
jarak p1 p2 = sqrt (fromIntegral ((absis p2 - absis p1)^2 + (ordinat p2 - ordinat p1)^2))

    -- Main Program
main :: IO()
main = do 
    x1 <- readLn :: IO Int
    y1 <- readLn :: IO Int
    x2 <- readLn :: IO Int
    y2 <- readLn :: IO Int
    n  <- readLn :: IO Int
    -- membuat point
    let p1 = makePoint x1 y1
        p2 = makePoint x2 y2
    putStrLn $ "p1 = " ++ show p1
    putStrLn $ "p2 = " ++ show p2
    putStrLn $ "n = " ++ show n
    -- memanggil fungsi isOnSbY(p)
    putStrLn $ "isOnSbY(p1) = " ++ show (isOnSbY p1)
    putStrLn $ "isOnSbY(p2) = " ++ show (isOnSbY p2)
    -- memanggil fungsi isEqual(p1, p2)
    putStrLn $ "isEqual(p1, p2) = " ++ show (isEqual p1 p2)
    -- memanggil fungsi translasiSbY(p, n)
    putStrLn $ "translasiSbY(p1, n) = " ++ show (translasiSbY p1 n)
    putStrLn $ "translasiSbY(p2, n) = " ++ show (translasiSbY p2 n)
    -- memanggil fungsi jarak(p1, p2)
    putStrLn $ "jarak(p1, p2) = " ++ show (jarak p1 p2)
        -- putStrLn $ "Kuadran p1 : " ++ kuadran p1 ++ " -> " ++ show p1
        -- putStrLn $ "Kuadran p2 : " ++ kuadran p2 ++ " -> " ++ show p2