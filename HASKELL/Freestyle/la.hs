-- Definisi tipe data Point
data Point = Point { x :: Int, y :: Int } deriving Show

-- realisasi selektor
absis :: Point -> Int
absis point = x point

ordinat :: Point -> Int
ordinat point = y point

-- realisasi konstruktor
makePoint :: Int -> Int -> Point
makePoint x y = Point { x = x, y = y }

-- realisasi predikat
isOrigin :: Point -> Bool
isOrigin p = absis p == 0 && ordinat p == 0

kuadran :: Point -> String
kuadran p
    | absis p > 0 && ordinat p > 0 = "1"
    | absis p < 0 && ordinat p > 0 = "2"
    | absis p < 0 && ordinat p < 0 = "3"
    | absis p > 0 && ordinat p < 0 = "4"
    | otherwise                    = "Lainnya"

main :: IO ()
main = do
    -- membuat point
    let p1 = makePoint 2 (-2)
        p2 = makePoint 2 2
        p3 = makePoint 0 0
        p4 = makePoint (-2) (-2)
        p5 = makePoint (-2) 2

    print (isOrigin p1)
    print (isOrigin p2)
    print (isOrigin p3)
    print (isOrigin p4)
    print (isOrigin p5)

    putStrLn (show (kuadran p1))
    putStrLn (show (kuadran p2))
    putStrLn (show (kuadran p3))
    putStrLn (show (kuadran p4))
    putStrLn (show (kuadran p5))