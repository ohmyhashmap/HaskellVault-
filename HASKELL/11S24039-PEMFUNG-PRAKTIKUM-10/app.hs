{- EVERYTHING IN FP IS EITHER A FUNCTION OR A DATA
Behavior, for example, is handled purely using functions in functional programming. Functions are “self contained” pieces of code that accomplish a specific task. It defines a relationship between a set of possible inputs and a set of possible outputs — they usually take in data, process it, and return a result. Once a function is written, it can be used over and over and over again.
IMMUTABLE MEANS EVERY FUNC IS MAKING A NEW RESULT
-}
{-NOTE : '_' underscore/placeholder/wildcard sebagai nilai yang diabaikan-}

{- KONSTRUKTOR -}

{- konso e li menghasilkan sebuah list of integer dari e dan li dengan e sebagai elemen pertama -}
konso :: Int -> [Int] -> [Int] 

konso e li = e : li

{- konsb li e menghasilkan sebuah list of integer dari li dan e dengan e sebagai elemen terakhir -}
konsb :: [Int] -> Int -> [Int]

konsb [] e = [e]               -- If the list is empty, return a list with the element e
konsb (x:xs) e = x : konsb xs e -- Otherwise, add the element x to the front of the recursive call


{- SELEKTOR -}

{- firstElmt(l), mengembalikan elemen pertama list l -}
firstElmt :: [Int] -> Int

firstElmt [] = 0
firstElmt (x:_) = x

{- tailList(l), mengembalikan list tanpa elemen pertama list l -}
tailList :: [Int] -> [Int]

tailList [] = []
tailList [x] = []
tailList (_:xs) = xs

{- lastElmt(l), mengembalikan elemen terakhir list l -}
lastElmt :: [Int] -> Int

lastElmt [] = 0
lastElmt [x] = x
lastElmt (_:xs) = lastElmt xs

{- headList(l), mengembalikan list tanpa elemen terakhir list l -} -- [3,3,4] -> [3,3]
headList :: [Int] -> [Int]

headList [] = []
headList [x] = []
headList (x : xs) = x : headList xs

{-midList(l), menghasilkan list untuk elemen yang berada di tengah list l -}
midList :: [Int] -> [Int]

midList [] = []
midList [x] = [x]
midList l = findMiddle l l  -- Memulai dengan dua  penunjuk di posisi awal

-- Fungsi rekursif untuk menemukan elemen tengah
findMiddle :: [Int] -> [Int] -> [Int]

findMiddle (_:_:xs) (_:ys) = findMiddle xs ys   -- `fast` melompat dua, `slow` melompat satu
findMiddle [x] _             = [x]                -- Satu elemen tengah untuk panjang ganjil
findMiddle (x:y:_) [_]       = [x, y]             -- Dua elemen tengah untuk panjang genap
findMiddle _ _               = []                 -- Untuk list kosong atau satu elemen

{-OPERATOR-}

{-nbElmt(l), menghitung jumlah elemen list, nol jika kosong.-}
nbElmt :: [a] -> Int

nbElmt [] = 0
nbElmt (_:xs) = 1 + nbElmt xs 

{-konkat(l1, l2), menghasilkan konkatenasi 2 buah list, dengan list l2 sesudah list l1 -}
konkat :: [Int] -> [Int] -> [Int]

konkat [] ys = ys -- jika list pertama kosong
konkat (x:xs) ys = x : konkat xs ys

-- {-reverseList(l), memberikan list yang urutan elemennya adalah kebalikan dari list asal-}
-- --Fungsi utama untuk membalik list
-- reverseList :: [Int] -> [Int]
-- reverseList [] = []                     -- Basis: list kosong tetap kosong
-- reverseList [x] = [x]                   -- Jika hanya ada satu elemen, kembalikan list yang sama
-- reverseList (x:xs) = reverseList xs `snoc` x  -- Rekursi dengan menambahkan elemen terakhir di depan

-- -- FUNGSI BANTU untuk menambah elemen di akhir list
-- {-cons: (Construct) menambahkan elemen ke depan list (misalnya, x : xs), yang biasa digunakan di Haskell.
-- snoc: (Singe-Negative-Cons) menambahkan elemen ke akhir list.-}
-- snoc :: [Int] -> Int -> [Int]
-- snoc [] y = [y]                         -- Jika list kosong, elemen baru menjadi satu-satunya elemen
-- snoc (x:xs) y = x : snoc xs y           -- Tambahkan elemen ke akhir list secara rekursif

reverseList :: [a] -> [a]

reverseList [] = []
reverseList [x] = [x]
reverseList (x:xs) = reverseList xs ++ [x]

{- maxList(l), menghasilkan nilai elemen list yang mempunyai nilai terbesar. Minimal list terdiri atas 1 elemen-}
maxList :: [Int] -> Int

maxList [x] = x  -- Basis: jika hanya ada satu elemen, maka elemen tersebut adalah nilai terbesar
maxList (x:xs) = max2 x (maxList xs)  -- Rekursi: bandingkan elemen pertama dengan nilai terbesar dari sisa list

--FUNGSI BANTU untuk mengambil nilai max dari list
max2 :: Int -> Int -> Int

max2 x y 
  | x > y = x
  | otherwise = y

{-elmtKeN(l, n) untuk mengembalikan nilai elemen pada posisi n di list l. Ukuran list harus lebih besar atau sama dengan n-}
elmtKeN :: [Int] -> Int -> Int

elmtKeN [] _ = 0
elmtKeN (x:_) 1 = x
elmtKeN (x:xs) n = elmtKeN xs (n-1)

{-sumList(l), menghasilkan total nilai dari elemen yang terdapat pada list l-}
sumList :: [Int] -> Int

sumList [] = 0
sumList (x:xs) = x + sumList xs

{-averageList(l), menghasilkan nilai rata-rata dari elemen yang terdapat pada list l.-}
averageList :: [Int] -> Float

averageList [] = 0
averageList l = fromIntegral (sumList l) / fromIntegral (nbElmt l)

{-PREDIKAT-}

{-isEmpty(l), adalah benar jika list kosong-}
isEmpty :: [Int] -> Bool

isEmpty [] = True
isEmpty (_:_) = False

{-isOneElemt(l), adalah benar jika list berisi satu elemen-}
isOneElemt :: [Int] -> Bool

isOneElemt [x] = True
isOneElemt _ = False

{-isEqual(l1, l2), adalah benar jika list l1 sama dengan list l2-}
isEqual :: Eq a => [a] -> [a] -> Bool

isEqual [] [] = True
isEqual (x:xs) (y:ys) = (x == y) && isEqual xs ys
isEqual _ _ = False

-- Predikat isMember: mengembalikan True jika elemen ada dalam list
isMember :: Eq a => [a] -> a -> Bool

isMember [] _ = False
isMember (x:xs) e = (x == e) || isMember xs e

-- Predikat isFirst: mengembalikan True jika elemen adalah elemen pertama
isFirst :: Eq a => [a] -> a -> Bool

isFirst (x:_) e = x == e
isFirst [] _ = False

-- Predikat isLast: mengembalikan True jika elemen adalah elemen terakhir
isLast :: Eq a => [a] -> a -> Bool

isLast [x] e = x == e
isLast (_:xs) e = isLast xs e
isLast [] _ = False

-- Predikat isNbElemt: mengembalikan True jika dua list memiliki jumlah elemen yang sama
isNbElemt :: [a] -> [a] -> Bool

isNbElemt l1 l2 = nbElmt l1 == nbElmt l2

-- Predikat isReverse: mengembalikan True jika satu list adalah kebalikan dari list lain
isReverse :: Eq a => [a] -> [a] -> Bool

isReverse l1 l2 = reverseList l1 == l2

-- Predikat isXElmtKeN: mengembalikan True jika elemen berada di posisi n
isXElmtKeN :: Eq a => [a] -> a -> Int -> Bool

isXElmtKeN (x:xs) e 1 = x == e
isXElmtKeN (x:xs) e n = isXElmtKeN xs e (n-1)
isXElmtKeN _ _ _ = False

{-getListFromInput(n), untuk menerima list dari inputan  -}
getListFromInput :: Int -> IO [Int]

getListFromInput 0 = return []
getListFromInput n = do
  x <- readLn :: IO Int
  xs <- getListFromInput (n - 1)
  return (x : xs)

main = do
  list1 <- readLn :: IO Int
  l1 <- getListFromInput list1
  list2 <- readLn :: IO Int
  l2 <- getListFromInput list2
  e <- readLn :: IO Int
  n1 <- readLn :: IO Int
  n2 <- readLn :: IO Int
  x1 <- readLn :: IO Int
  x2 <- readLn :: IO Int

  putStrLn $ "e = " ++ show e
  putStrLn $ "l1 = " ++ show l1
  putStrLn $ "l1 (konso) = " ++ show (konso e l1)
  putStrLn $ "l1 (konsb) = " ++ show (konsb l1 e)
  putStrLn $ "l2 = " ++ show l2
  putStrLn $ "l2 (konso) = " ++ show (konso e l2)
  putStrLn $ "l2 (konsb) = " ++ show (konsb l2 e)
  putStrLn $ "n1 = " ++ show n1
  putStrLn $ "n2 = " ++ show n2
  putStrLn $ "x1 = " ++ show x1
  putStrLn $ "x2 = " ++ show x2

  putStrLn $ "firstElmt(l1) = " ++ show (firstElmt l1)
  putStrLn $ "firstElmt(l2) = " ++ show (firstElmt l2)
  putStrLn $ "tailList(l1) = " ++ show (tailList l1)
  putStrLn $ "tailList(l2) = " ++ show (tailList l2)
  putStrLn $ "lastElmt(l1) = " ++ show (lastElmt l1)
  putStrLn $ "lastElmt(l2) = " ++ show (lastElmt l2)
  putStrLn $ "headList(l1) = " ++ show (headList l1)
  putStrLn $ "headList(l2) = " ++ show (headList l2)
  putStrLn $ "midList(l1) = " ++ show (midList l1)
  putStrLn $ "midList(l2) = " ++ show (midList l2)
  putStrLn $ "nbElmt(l1) = " ++ show (nbElmt l1)
  putStrLn $ "nbElmt(l2) = " ++ show (nbElmt l2)
  putStrLn $ "konkat(l1, l2) = " ++ show (konkat l1 l2)
  putStrLn $ "maxList(l1) = " ++ show (maxList l1)
  putStrLn $ "maxList(l2) = " ++ show (maxList l2)
  -- putStrLn $ "reverseList(l1) = " ++ show (reverseList l1)
  -- putStrLn $ "reverseList(l2) = " ++ show (reverseList l2)
  putStrLn $ "elmtKeN(l1, n1) = " ++ show (elmtKeN l1 n1)
  putStrLn $ "elmtKeN(l2, n2) = " ++ show (elmtKeN l2 n2)
  putStrLn $ "sumList(l1) = " ++ show (sumList l1)
  putStrLn $ "sumList(l2) = " ++ show (sumList l2)
  putStrLn $ "averageList(l1) = " ++ show (averageList l1)
  putStrLn $ "averageList(l2) = " ++ show (averageList l2)
  -- putStrLn $ "isEmpty(l1) = " ++ show (isEmpty l1)
  -- putStrLn $ "isOneElemt(l1) = " ++ show (isOneElemt l1)
  putStrLn $ "isEqual(l1, l2) = " ++ show (isEqual l1 l2)
  putStrLn $ "isEqual(l1, l2) = " ++ show (isEqual l1 l2)
  putStrLn $ "isMember(l1, x1) = " ++ show (isMember l1 x1)
  putStrLn $ "isMember(l2, x2) = " ++ show (isMember l2 x2)
  putStrLn $ "isFirst(l1, x1) = " ++ show (isFirst l1 x1)
  putStrLn $ "isFirst(l2, x2) = " ++ show (isFirst l2 x2)
  putStrLn $ "islast(l1, x1) = " ++ show (isLast l1 x1)
  putStrLn $ "islast(l2, x2) = " ++ show (isLast l2 x2)
  putStrLn $ "isNbElemt(l1, l2) = " ++ show (isNbElemt l1 l2)
  putStrLn $ "isReverse(l1, l2) = " ++ show (isReverse l1 l2)
  putStrLn $ "isXElmtKeN(l1, x1, n1) = " ++ show (isXElmtKeN l1 x1 n1)
  putStrLn $ "isXElmtKeN(l2, x2, n2) = " ++ show (isXElmtKeN l2 x2 n2)  