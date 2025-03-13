module Date where
-- Define the Date data type with day, month, and year
--Adding Eq to the deriving clause allows Haskell to automatically generate an instance of the Eq typeclass,
-- so you can use == and /= to compare Date values. After making this change, isNext should work without any issues.
-- Tipe data untuk Date
data Date = Date { dd :: Int, mm :: Int, yy :: Int } deriving Show

-- Konstruktor
-- makeDate(dd, mm, yy), untuk membentuk Date, dari hari dd, bulan mm, dan tahun yy
makeDate :: Int -> Int -> Int -> Date
makeDate dd mm yy = Date { dd = dd, mm = mm, yy = yy }

-- Selektor
-- day(d), untuk mengembalikan nilai hari (dd) dari Date d
day :: Date -> Int
day date = dd date 

-- month(d), untuk mengembalikan nilai bulan (mm) dari Date d
month :: Date -> Int
month date = mm date 

-- year(d), untuk mengembalikan nilai tahun (yy) dari Date d
year :: Date -> Int
year date = yy date 

-- Operator
-- Fungsi untuk mendapatkan tanggal berikutnya
nextDay :: Date -> Date
nextDay d = Date { dd = if day d == 29 && isKabisat (year d) && month d == 2 then 1
            else if day d == 28 && isKabisat (year d) && month d == 2 then 29
            else if day d == 28 && not (isKabisat (year d)) && month d == 2 then 1
            else if day d == 30 && month31 (month d) then 31
            else if day d == 30 && month30 (month d) then 1
            else if day d == 31 && month31 (month d) then 1
            else day d + 1,   
            mm = if day d == 31 && month d == 12 then 1
                else if day d == 29 && isKabisat (year d) && month d == 2 then month d + 1
                else if day d == 28 && not (isKabisat (year d)) && month d == 2 then month d + 1
                else if day d == 28 && isKabisat (year d) && month d == 2 then month d
                else if day d == 30 && month30 (month d) then month d + 1
                else if day d == 31 && month31 (month d) then month d + 1 
                else month d, -- Default value for month
            yy = if (month d == 12) && (day d == 31) then year d + 1
                else year d}

-- Fungsi untuk mendapatkan tanggal sebelumnya
yesterday :: Date -> Date
yesterday d =
    Date { dd = if day d == 1 && (month d == 1) && year d == 1 then 1
                else if day d == 1 && month d == 2 then 31
                else if day d == 1 && (month d == 3) && isKabisat(year d) then 29
                else if day d == 1 && (month d == 3) then 28
                else if day d == 1 && (month d == 1) then 31
                else if day d <= 31 && day d > 1 then day d - 1
                else if day d == 1 && month30 (month d) then 31
                else if day d == 1 && month31 (month d) then 30
                else 30 ,
            mm = if day d == 1 && (month d == 1) && year d == 1 then 1
                else if day d == 1 && (month d == 3) && isKabisat(year d)  then 2 
                else if day d == 1 && (month d == 1) then 12
                else if day d == 1 && month d <= 12 && month d > 1 then month d - 1
                else month d ,
            yy = if day d == 1 && (month d == 1) && year d == 1 then 1
                else if day d == 1 && (month d == 1) then year d - 1
                else year d}

-- Fungsi untuk menambahkan hari
addDay :: Date -> Int -> Date
addDay d n
    | n == 0 = d
    | n > 0 = addDay (nextDay d) (n - 1)


-- Fungsi untuk mengurangi hari
subDay :: Date -> Int -> Date
subDay d n
    | n == 0 = d
    | n > 0 = subDay (yesterday d) (n - 1)

-- Fungsi untuk mendapatkan bulan berikutnya
nextMonth :: Date -> Date
nextMonth d = Date { dd = day d
                    , mm = if (month d + 1) <= 12 then month d + 1 
                            else (month d + 1) `mod` 12
                    , yy = if (month d + 1) <= 12 then year d
                            else year d + 1
                    }

-- Fungsi untuk mendapatkan bulan sebelumnya
prevMonth :: Date -> Date
prevMonth d = Date { dd = if (month d == 1) && year d == 1 then 1
                            else day d , 
                    mm = if (month d == 1) && year d == 1 then 1 
                            else if month d > 1 then month d - 1
                            else 12 ,
                    yy = if (month d == 1) && year d == 1 then 1
                            else if month d == 1 then year d - 1
                            else year d }

-- Fungsi untuk menambahkan bulan
addMonth :: Date -> Int -> Date
addMonth d n = if n == 0 then d
        else addMonth (nextMonth d) (n-1)

-- Fungsi untuk mengurangi bulan
subMonth :: Date -> Int -> Date
subMonth d n = if n == 0 then d
                else subMonth (prevMonth d) (n-1)

-- Fungsi untuk menambahkan tahun
addYear :: Date -> Int -> Date
addYear d n = Date {dd= day d, mm = month d, yy= year d + n }

-- Fungsi untuk mengurangi tahun
subYear :: Date -> Int -> Date
subYear d n = Date {dd = if n >= year d then 1
                            else day d, 
                    mm = if n >= year d then 1
                            else month d,
                    yy = if  n >= year d then 1
                            else year d - n}

-- Memeriksa tahun kabisat
isKabisat :: Int -> Bool
isKabisat yy
    | yy `mod` 400 == 0 = True
    | yy `mod` 100 == 0 = False
    | yy `mod` 4 == 0 = True
    | otherwise = False

-- Memeriksa bulan
month30 :: Int -> Bool
month30 m = m `elem` [4, 6, 9, 11]

month31 :: Int -> Bool
month31 m = m `elem` [1, 3, 5, 7, 8, 10, 12]

-- Predikat
-- Fungsi untuk membandingkan kesetaraan dua tanggal
isEqD :: Date -> Date -> Bool
isEqD d1 d2 = year d1 == year d2 && month d1 == month d2 && day d1 == day d2 

-- Fungsi untuk membandingkan ketidaksetaraan dua tanggal
isNotEqD :: Date -> Date -> Bool
isNotEqD d1 d2 = not (isEqD d1 d2) 

-- Fungsi untuk memeriksa apakah d1 sebelum d2
isBefore :: Date -> Date -> Bool
isBefore d1 d2 = day d1 == day (yesterday d2) && month d1 == month (yesterday d2) && year d1 == year (yesterday d2)

-- Fungsi untuk memeriksa apakah d1 setelah d2
isNext :: Date -> Date -> Bool
isNext d1 d2 = day d1 == day (nextDay d2) && month d1 == month (nextDay d2) && year d1 == year (nextDay d2)

-- Fungsi untuk memeriksa apakah d1 sebelum d2 sebanyak n
isBeforeN :: Date -> Date -> Int -> Bool
isBeforeN d1 d2 n = isEqD (addDay d1 n) d2

-- Fungsi untuk memeriksa apakah d1 setelah d2 sebanyak n
isNextN :: Date -> Date -> Int -> Bool
isNextN d1 d2 n = isEqD (subDay d1 n) d2

-- Fungsi utama untuk interaksi
main :: IO ()
main = do
    dd1 <- readLn :: IO Int
    mm1 <- readLn :: IO Int
    yy1 <- readLn :: IO Int
    dd2 <- readLn :: IO Int
    mm2 <- readLn :: IO Int
    yy2 <- readLn :: IO Int
    n <- readLn :: IO Int

    let d1 = makeDate dd1 mm1 yy1
        d2 = makeDate dd2 mm2 yy2

    putStrLn $ "d1 = " ++ show d1
    putStrLn $ "d2 = " ++ show d2
    putStrLn $ "n = " ++ show n
    putStrLn $ "nextDay(d1) = " ++ show (nextDay d1)
    putStrLn $ "nextDay(d2) = " ++ show (nextDay d2)
    putStrLn $ "yesterday(d1) = " ++ show (yesterday d1)
    putStrLn $ "yesterday(d2) = " ++ show (yesterday d2)
    putStrLn $ "addDay(d1, n) = " ++ show (addDay d1 n)
    putStrLn $ "addDay(d2, n) = " ++ show (addDay d2 n)
    putStrLn $ "subDay(d1, n) = " ++ show (subDay d1 n)
    putStrLn $ "subDay(d2, n) = " ++ show (subDay d2 n)
    putStrLn $ "nextMonth(d1) = " ++ show (nextMonth d1)
    putStrLn $ "nextMonth(d2) = " ++ show (nextMonth d2)
    putStrLn $ "prevMonth(d1) = " ++ show (prevMonth d1)
    putStrLn $ "prevMonth(d2) = " ++ show (prevMonth d2)
    putStrLn $ "addMonth(d1, n) = " ++ show (addMonth d1 n)
    putStrLn $ "addMonth(d2, n) = " ++ show (addMonth d2 n)
    putStrLn $ "subMonth(d1, n) = " ++ show (subMonth d1 n)
    putStrLn $ "subMonth(d2, n) = " ++ show (subMonth d2 n)
    putStrLn $ "addYear(d1, n) = " ++ show (addYear d1 n)
    putStrLn $ "addYear(d2, n) = " ++ show (addYear d2 n)
    putStrLn $ "subYear(d1, n) = " ++ show (subYear d1 n)
    putStrLn $ "subYear(d2, n) = " ++ show (subYear d2 n)
    putStrLn $ "isEqD(d1, d2) = " ++ show (isEqD d1 d2)
    putStrLn $ "isNotEqD(d1, d2) = " ++ show (isNotEqD d1 d2)
    putStrLn $ "isBefore(d1, d2) = " ++ show (isBefore d1 d2)
    putStrLn $ "isNext(d1, d2) = " ++ show (isNext d1 d2)
    putStrLn $ "isBeforeN(d1, d2, n) = " ++ show (isBeforeN d1 d2 n)
    putStrLn $ "isNextN(d1, d2, n) = " ++ show (isNextN d1 d2 n)
    -- Selesai