-- EVERYTHING IN FP IS A FUNCTION
-- IMMUTABLE MEANS EVERY FUNC IS MAKING A NEW RESULT 
{-NOTE : '_' underscore/placeholder/wildcard sebagai nilai yang diabaikan-}

{-KONSTRUKTOR-}

{- konso e li menghasilkan sebuah list of integer dari e dan li dengan e sebagai elemen pertama -}
konso :: Int -> [Int] -> [Int] 

konso e (x:xs) = e:(x:xs)

{- konsb li e menghasilkan sebuah list of integer dari li dan3
e dengan e sebagai elemen terakhir -}
konsb :: [Int] -> Int -> [Int]

konsb e (x:xs) = x : konsb xs e

{-SELEKTOR-}

{- firstElmt(l), mengembalikan elemen pertama list l -}
firstElmt :: [Int] -> Int

firstElmt [] = 0
firstElmt (x:xs) = x

{- tailList(l), mengembalikan list tanpa elemen pertama list l -}
tailList :: [Int] -> [Int]

tailList [] = 0
tailList (x:xs) =  : tailList xs

{- lastElmt(l), mengembalikan elemen terakhir list l -}
lastElmt :: [Int] -> Int

lastElmt [] = 0
lastElmt [x] = x
lastElmt (_:xs) = lastElmt xs 

{- headList(l), mengembalikan list tanpa elemen terakhir list l -} -- [3,3,4] -> [3,3]
headList :: [Int] -> [Int]

headList [] = 0
headList (x:xs) = 

{-midList(l), menghasilkan list untuk elemen yang berada di tengah list l -}
midList :: [Int] -> [Int]


-- Fungsi rekursif untuk menemukan elemen tengah
findMiddle :: [Int] -> [Int] -> [Int]

{-OPERATOR-}

{-nbElmt(l), menghitung jumlah elemen list, nol jika kosong.-}
nbElmt :: [a] -> Int


{-konkat(l1, l2), menghasilkan konkatenasi 2 buah list, dengan list l2 sesudah list l1 -}
konkat :: [Int] -> [Int] -> [Int]


-- {-reverseList(l), memberikan list yang urutan elemennya adalah kebalikan dari list asal-}
-- -- Fungsi utama untuk membalik list
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


{- maxList(l), menghasilkan nilai elemen list yang mempunyai nilai terbesar. Minimal list terdiri atas 1 elemen-}
maxList :: [Int] -> Int


--FUNGSI BANTU untuk mengambil nilai max dari list
max2 :: Int -> Int -> Int
max2 x y 
  | x > y = x
  | otherwise = y

{-elmtKeN(l, n) untuk mengembalikan nilai elemen pada posisi n di list l. Ukuran list harus lebih besar atau sama dengan n-}
elmtKeN :: [Int] -> Int -> Int


{-sumList(l), menghasilkan total nilai dari elemen yang terdapat pada list l-}
sumList :: [Int] -> Int

{-averageList(l), menghasilkan nilai rata-rata dari elemen yang terdapat pada list l.-}
averageList :: [Int] -> Float


{-PREDIKAT-}

{-isEmpty(l), adalah benar jika list kosong-}
isEmpty :: [Int] -> Bool


{-isOneElemt(l), adalah benar jika list berisi satu elemen-}
isOneElemt :: [Int] -> Bool


{-isEqual(l1, l2), adalah benar jika list l1 sama dengan list l2-}
isEqual :: Eq a => [a] -> [a] -> Bool


-- Predikat isMember: mengembalikan True jika elemen ada dalam list
isMember :: Eq a => [a] -> a -> Bool


-- Predikat isFirst: mengembalikan True jika elemen adalah elemen pertama
isFirst :: Eq a => [a] -> a -> Bool

-- Predikat isLast: mengembalikan True jika elemen adalah elemen terakhir
isLast :: Eq a => [a] -> a -> Bool

-- Predikat isNbElemt: mengembalikan True jika dua list memiliki jumlah elemen yang sama
isNbElemt :: [a] -> [a] -> Bool


-- Predikat isReverse: mengembalikan True jika satu list adalah kebalikan dari list lain
isReverse :: Eq a => [a] -> [a] -> Bool


-- Predikat isXElmtKeN: mengembalikan True jika elemen berada di posisi n
isXElmtKeN :: Eq a => [a] -> a -> Int -> Bool


{-getListFromInput(n), untuk menerima list dari inputan  -}
getListFromInput :: Int -> IO [Int]
getListFromInput 0 = return []
getListFromInput n = do
  x <- readLn :: IO Int
  xs <- getListFromInput (n - 1)
  return (x : xs)

main = do
  e <- readLn :: IO Int
  l1 <- getListFromInput e
  list2 <- readLn :: IO Int
  l2 <- getListFromInput list2
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