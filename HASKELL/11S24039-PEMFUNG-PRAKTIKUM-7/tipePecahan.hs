-- Definisikan tipe data Pecahan
data Pecahan = Pecahan { a :: Int, b :: Int } deriving Show

-- Konstruktor
makePecahan :: Int -> Int -> Pecahan
makePecahan x y = Pecahan { a = x, b = y }

-- Selektor 
getPembilang :: Pecahan -> Int
getPembilang v = a v

getPenyebut :: Pecahan -> Int
getPenyebut v = b v

-- Operator Pecahan
addP :: Pecahan -> Pecahan -> Pecahan
addP v w = makePecahan (getPembilang v * getPenyebut w + getPembilang w * getPenyebut v) 
                        (getPenyebut v * getPenyebut w)

subP :: Pecahan -> Pecahan -> Pecahan
subP v w = makePecahan (getPembilang v * getPenyebut w - getPembilang w * getPenyebut v) 
                        (getPenyebut v * getPenyebut w)

mulP :: Pecahan -> Pecahan -> Pecahan
mulP v w = makePecahan (getPembilang v * getPembilang w) 
                        (getPenyebut v * getPenyebut w)

divP :: Pecahan -> Pecahan -> Pecahan
divP v w = makePecahan (getPembilang v * getPenyebut w) 
                        (getPenyebut v * getPembilang w)

-- Predikat (operator relasional pecahan)
isEqP :: Pecahan -> Pecahan -> Bool
isEqP v w = (getPembilang v * getPenyebut w) == (getPembilang w * getPenyebut v)

isLtP :: Pecahan -> Pecahan -> Bool
isLtP v w = (getPembilang v * getPenyebut w) < (getPembilang w * getPenyebut v)

-- Fungsi utama
main :: IO ()
main = do
    -- Input dari pengguna
    x1 <- readLn :: IO Int
    y1 <- readLn :: IO Int
    x2 <- readLn :: IO Int
    y2 <- readLn :: IO Int

    -- Membuat dua pecahan
    let pecahan1 = makePecahan x1 y1
    let pecahan2 = makePecahan x2 y2

    -- Menampilkan hasil operasi
    putStrLn $ "p1 = " ++ show pecahan1
    putStrLn $ "p2 = " ++ show pecahan2

    -- Hasil operasi pecahan
    putStrLn $ "addP(p1, p2) = " ++ show (addP pecahan1 pecahan2)
    putStrLn $ "subP(p1, p2) = " ++ show (subP pecahan1 pecahan2)
    putStrLn $ "mulP(p1, p2) = " ++ show (mulP pecahan1 pecahan2)
    putStrLn $ "divP(p1, p2) = " ++ show (divP pecahan1 pecahan2)
    putStrLn $ "isEqP(p1, p2) = " ++ show (isEqP pecahan1 pecahan2)
    putStrLn $ "isLtP(p1, p2) = " ++ show (isLtP pecahan1 pecahan2)
