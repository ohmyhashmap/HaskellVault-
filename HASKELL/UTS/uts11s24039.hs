-- Definisikan Vector3 data 
data Vector3 = Vector3 { x :: Double, y :: Double, z :: Double } deriving Show

-- konstruktor
-- Fungsi untuk membuat vektor 3D
makeVector3 :: Double -> Double -> Double -> Vector3
makeVector3 x y z = Vector3 { x = x , y = y, z = z }

-- Selektor untuk x, y, z
getX :: Vector3 -> Double
getX vector = x vector

getY :: Vector3 -> Double
getY vector = y vector

getZ :: Vector3 -> Double
getZ vector = z vector

-- Fungsi untuk memeriksa apakah vektor adalah vektor nol (origin)
isOrigin :: Vector3 -> Bool
isOrigin v = getX v == 0 && getY v == 0 && getZ v == 0

-- Fungsi untuk memeriksa kesamaan dua vektor
isEquals :: Vector3 -> Vector3 -> Bool
isEquals v w = getX v == getX w && getY v == getY w && getZ v == getZ w

-- Fungsi untuk memeriksa ketidaksamaan dua vektor
isNotEquals :: Vector3 -> Vector3 -> Bool
isNotEquals v w = not (isEquals v w)

-- Fungsi untuk mencari bilangan terbesar diantara 2 bilangan
max2 :: Double -> Double -> Double  
max2 x y = if x > y 
            then x 
            else y 

-- Fungsi untuk mencari bilangan terkecil diantara 2 bilangan
min2 :: Double -> Double -> Double  
min2 x y = if x <= y 
            then x 
            else y 

-- Fungsi untuk mencari komponen terbesar antara dua vektor
vectorMax :: Vector3 -> Vector3 -> Vector3
vectorMax v w = makeVector3 (max2 (getX v) (getX w)) (max2 (getY v) (getY w)) (max2 (getZ v) (getZ w))

-- Fungsi untuk mencari komponen terkecil antara dua vektor
vectorMin :: Vector3 -> Vector3 -> Vector3
vectorMin v w = makeVector3 (min2 (getX v) (getX w)) (min2 (getY v) (getY w)) (min2 (getZ v) (getZ w))

-- Fungsi untuk menghitung magnitudo (panjang) vektor
magnitude :: Vector3 -> Double
magnitude v = roundToTwo (sqrt ((getX v)^2 + (getY v)^2 + (getZ v)^2))

-- Fungsi untuk menghitung dot product dari dua vektor
dotProduct :: Vector3 -> Vector3 -> Double
dotProduct v w = roundToTwo (getX v * getX w) + (getY v * getY w) + (getZ v * getZ w)

-- Fungsi untuk menghitung cross product dari dua vektor
crossProduct :: Vector3 -> Vector3 -> Vector3
crossProduct v w = makeVector3 
    ((getY v * getZ w) - (getZ v * getY w))
    ((getZ v * getX w) - (getX v * getZ w))
    ((getX v * getY w) - (getY v * getX w))

-- Fungsi untuk menormalisasi vektor
normalized :: Vector3 -> Vector3
normalized v = 
    let mag = magnitude v
    in if mag == 0
        then makeVector3 (0) (0) (0)        
        -- then makeVector3 (-1/0) (-1/0) (-1/0)  -- Tangani kasus magnitudo nol\
        else makeVector3(roundToTwo (getX v / mag))
                        (roundToTwo (getY v / mag))
                        (roundToTwo (getZ v / mag))

-- Fungsi untuk menghitung sudut antara dua vektor dalam radian
angle :: Vector3 -> Vector3 -> Double
angle v w = roundToTwo( acos (roundToTwo ((dotProduct v w) / ((magnitude v) * (magnitude w)))))

-- Fungsi untuk mengonversi sudut radian ke derajat
anglerad :: Double -> Double
anglerad radian = roundToOne (radian * (180 / pi ))

-- Fungsi untuk menghitung jarak antara dua vektor
distance :: Vector3 -> Vector3 -> Double
distance v w = roundToTwo( sqrt ((getX w - getX v)^2 + (getY w - getY v)^2 + (getZ w - getZ v)^2))

-- Fungsi untuk melakukan scaling pada vektor
scale :: Vector3 -> Double -> Vector3
scale v t = makeVector3 (getX v * t) (getY v * t) (getZ v * t)

-- Fungsi untuk menjumlahkan dua vektor
addVector :: Vector3 -> Vector3 -> Vector3
addVector v w = makeVector3 (getX v + getX w) (getY v + getY w) (getZ v + getZ w)

-- Fungsi untuk mengurangkan dua vektor
subVector :: Vector3 -> Vector3 -> Vector3
subVector v w = makeVector3 (getX v - getX w) (getY v - getY w) (getZ v - getZ w)

-- Fungsi untuk melakukan perkalian komponen dua vektor
mulVector :: Vector3 -> Vector3 -> Vector3
mulVector v w = makeVector3 (getX v * getX w) (getY v * getY w) (getZ v * getZ w)

-- Fungsi untuk membagi komponen dua vektor, menangani pembagian dengan nol
divVector :: Vector3 -> Vector3 -> Vector3
divVector v w = makeVector3 
    (roundToOne (safeDiv (getX v) (getX w))) 
    (roundToOne (safeDiv (getY v) (getY w))) 
    (roundToOne (safeDiv (getZ v) (getZ w)))

-- Fungsi pembantu untuk menangani pembagian dengan nol
safeDiv :: Double -> Double -> Double
-- safeDiv 0 0 = 0         -- Mengembalikan 0 untuk menghindari NaN pada kasus 0/0
-- safeDiv a 0 = (1 / 0)   -- Mengembalikan Infinity jika penyebut adalah nol
safeDiv a b = a / b     -- Pembagian normal

-- Fungsi untuk menghitung pantulan vektor
reflect :: Vector3 -> Vector3 -> Vector3
reflect v w = subVector v (scale w (2 * dotProduct v w))

-- Fungsi untuk melakukan interpolasi linear antara dua vektor
lerp :: Vector3 -> Vector3 -> Double -> Vector3
lerp v w t = addVector (scale v (1 - t)) (scale w t)

-- Fungsi untuk membulatkan nilai ke beberapa tempat desimal
roundToTwo :: Double -> Double
roundToTwo x = fromIntegral (round (x * 100)) / 100

roundToOne :: Double -> Double
roundToOne x = fromIntegral (round (x * 10)) / 10

-- roundToN :: Double -> Double -> Double
-- roundToN x n = fromIntegral (round (x * 10^n)) /10^n

-- Main Program
main :: IO ()
main = do 
    x1 <- readLn :: IO Double -- memakai IO untuk readLn melakukan tindakan input dan output 
    y1 <- readLn :: IO Double -- bila tidak digunakan maka tidak akan melakukan aksi IO (Input Output)
    z1 <- readLn :: IO Double -- 
    x2 <- readLn :: IO Double
    y2 <- readLn :: IO Double
    z2 <- readLn :: IO Double
    t  <- readLn :: IO Double
    
    let v = makeVector3 x1 y1 z1
        w = makeVector3 x2 y2 z2
    
    putStrLn $ "v = " ++ show v
    putStrLn $ "w = " ++ show w
    putStrLn $ "t = " ++ show t
    putStrLn $ "isOrigin(v) = " ++ show (isOrigin v)
    putStrLn $ "isOrigin(w) = " ++ show (isOrigin w)
    putStrLn $ "isEquals(v, w) = " ++ show (isEquals v w)
    putStrLn $ "isNotEquals(v, w) = " ++ show (isNotEquals v w)
    putStrLn $ "vectorMax(v, w) = " ++ show (vectorMax v w)
    putStrLn $ "vectorMin(v, w) = " ++ show (vectorMin v w)
    putStrLn $ "magnitude(v) = " ++ show ( magnitude v )
    putStrLn $ "magnitude(w) = " ++ show ( magnitude w )
    putStrLn $ "normalized(v) = " ++ show (normalized v)
    putStrLn $ "normalized(w) = " ++ show (normalized w)
    putStrLn $ "dotProduct(v, w) = " ++ show (dotProduct v w)
    putStrLn $ "crossProduct(v, w) = " ++ show (crossProduct v w)
    putStrLn $ "angle(v, w) [radian] = " ++ show (angle v w) 
    putStrLn $ "angle(v, w) [degree] = " ++ show (anglerad (angle v w)) 
    putStrLn $ "distance(v, w) = " ++ show (distance v w)
    putStrLn $ "scale(v, t) = " ++ show (scale v t)
    putStrLn $ "addVector(v, w) = " ++ show (addVector v w)
    putStrLn $ "subVector(v, w) = " ++ show (subVector v w)
    putStrLn $ "mulVector(v, w) = " ++ show (mulVector v w)
    putStrLn $ "divVector(v, w) = " ++ show (divVector v w)
    putStrLn $ "reflect(v, w) = " ++ show (reflect v w)
    putStrLn $ "lerp(v, w, t) = " ++ show (lerp v w t)