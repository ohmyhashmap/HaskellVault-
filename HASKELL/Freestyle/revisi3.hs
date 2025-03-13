data Date = Date {dd :: Int, mm:: Int, yy :: Int} deriving Show

--selektor
day :: Date -> Int
day date = dd date 

month :: Date -> Int
month date = mm date 

year :: Date -> Int
year date = yy date 

-- konstruktor
makeDate :: Int -> Int -> Int -> Date
makeDate dd mm yy = Date {dd = dd, mm = mm, yy = yy}

-- Operator
nextday :: Date -> Date
nextday d = Date { dd = if day d == 29 && isKabisat (year d) && month d == 2 then 1
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

addday :: Date -> Int -> Date -- pakai rekursi untuk operator menambahkan n hari ke tanggal
-- pastikan ketentuan tanggal, misla 29 hari kalau kabisat
addday d n
     | n == 0 = d
     | n > 0 = addday (nextday d) (n - 1)

subday :: Date -> Int -> Date
subday d n
     | n == 0 = d
     | n > 0 = subday (yesterday d) (n - 1)

nextmonth :: Date -> Date
nextmonth d = Date { dd = day d
                    , mm = if (month d + 1) <= 12 then month d + 1 
                              else (month d + 1) `mod` 12
                    , yy = if (month d + 1) <= 12 then year d
                              else year d + 1
                    }

prevmonth :: Date -> Date
prevmonth d = Date { dd = if (month d == 1) && year d == 1 then 1
                         else day d , 
                    mm = if (month d == 1) && year d == 1 then 1 
                         else if month d > 1 then month d - 1
                         else 12 ,
                    yy = if (month d == 1) && year d == 1 then 1
                         else if month d == 1 then year d - 1
                         else year d }

addmonth :: Date -> Int -> Date
addmonth d n = if n == 0 then d
          else addmonth (nextmonth d) (n-1)

submonth :: Date -> Int -> Date
submonth d n = if n == 0 then d
               else submonth (prevmonth d) (n-1)

addyear :: Date -> Int -> Date
addyear d n = Date {dd= day d, mm = month d, yy= year d + n }

subyear :: Date -> Int -> Date
subyear d n = Date {dd = if n >= year d then 1
                         else day d, 
                    mm = if n >= year d then 1
                         else month d,
                    yy = if  n >= year d then 1
                         else year d - n}

--selektor
isequal :: Date -> Date -> Bool
isequal d1 d2= year d1 == year d2 && month d1 == month d2 && day d1 == day d2 

isnotequal :: Date -> Date -> Bool
isnotequal d1 d2 = not (isequal d1 d2) 

-- isNext(d1, d2), mengembalikan nilai benar jika tanggal d1 sesudah d2.
isNext :: Date -> Date -> Bool
isNext d1 d2 = day d1 == day (nextday d2) && month d1 == month (nextday d2) && year d1 == year (nextday d2)

-- isBefore(d1, d2), mengembalikan nilai benar jika tanggal d1 sebelum d2.
isBefore :: Date -> Date -> Bool
isBefore d1 d2 = day d1 == day (yesterday d2) && month d1 == month (yesterday d2) && year d1 == year (yesterday d2)

-- isBeforeN(d1, d2, n), mengembalikan nilai benar jika tanggal d1 sebelum d2 sebanyak n.
isBeforeN :: Date -> Date -> Int -> Bool
isBeforeN d1 d2 n = isequal (addday d1 n) d2

-- isNextN(d1, d2, n), mengembalikan nilai benar jika tanggal d1 sesudah d2 sebanyak n.
isNextN :: Date -> Date -> Int -> Bool
isNextN d1 d2 n = isequal (subday d1 n) d2

isKabisat :: Int -> Bool
isKabisat yy
          | yy `mod` 400 == 0 = True
          | yy `mod` 4 == 0 && yy `mod` 100 /= 0 = True
          | otherwise = False

month30 :: Int -> Bool
month30 m = m `elem` [4, 6, 9, 11]

month31 :: Int -> Bool
month31 m = m `elem` [1, 3, 5, 7, 8, 10, 12]

main :: IO ()
main = do
          dd1 <- readLn :: IO Int
          mm1 <- readLn :: IO Int
          yy1 <- readLn :: IO Int
          dd2 <- readLn :: IO Int
          mm2 <- readLn :: IO Int
          yy2 <- readLn :: IO Int
          n <- readLn :: IO Int
          -- membuat date
          let d1 = makeDate dd1 mm1 yy1
               d2 = makeDate dd2 mm2 yy2

          putStrLn $ "d1 = " ++ show d1
          putStrLn $ "d2 = " ++ show d2
          putStrLn $ "n = " ++ show n
          -- memanggil fungsi nextDay(d)
          putStrLn $ "nextDay(d1) = " ++ show (nextday d1)
          putStrLn $ "nextDay(d2) = " ++ show (nextday d2)
          -- memanggil fungsi yesterday(d)
          putStrLn $ "yesterday(d1) = " ++ show (yesterday d1)
          putStrLn $ "yesterday(d2) = " ++ show (yesterday d2)

          putStrLn $ "addDay(d1, n) = " ++ show (addday d1 n)
          putStrLn $ "addDay(d2, n) = " ++ show (addday d2 n)

          -- memanggil fungsi subDay(d, n)
          putStrLn $ "subDay(d1, n) = " ++ show (subday d1 n)
          putStrLn $ "subDay(d2, n) = " ++ show (subday d2 n)
          
          -- memanggil fungsi nextMonth(d)
          putStrLn $ "nextMonth(d1) = " ++ show (nextmonth d1)
          putStrLn $ "nextMonth(d2) = " ++ show (nextmonth d2)
          -- memanggil fungsi prevMonth(d)
          putStrLn $ "prevMonth(d1) = " ++ show (prevmonth d1)
          putStrLn $ "prevMonth(d2) = " ++ show (prevmonth d2)
          -- memanggil fungsi addMonth(d, n)
          putStrLn $ "addMonth(d1, n) = " ++ show (addmonth d1 n)
          putStrLn $ "addMonth(d2, n) = " ++ show (addmonth d2 n)
          -- memanggil fungsi subMonth(d, n)
          putStrLn $ "subMonth(d1, n) = " ++ show (submonth d1 n)
          putStrLn $ "subMonth(d2, n) = " ++ show (submonth d2 n)
          -- memanggil fungsi addYear(d, n)
          putStrLn $ "addYear(d1, n) = " ++ show (addyear d1 n)
          putStrLn $ "addYear(d2, n) = " ++ show (addyear d2 n)
          -- memanggil fungsi subYear(d, n)
          putStrLn $ "subYear(d1, n) = " ++ show (subyear d1 n)
          putStrLn $ "subYear(d2, n) = " ++ show (subyear d2 n)
          -- memanggil fungsi isEqD(d1, d2)
          putStrLn $ "isEqD(d1, d2) = " ++ show (isequal d1 d2)
          -- memanggil fungsi isNotEqD(d1, d2)
          putStrLn $ "isNotEqD(d1, d2) = " ++ show (isnotequal d1 d2)
          -- memanggil fungsi isBefore(d1, d2)
          putStrLn $ "isBefore(d1, d2) = " ++ show (isBefore d1 d2)
          -- memanggil fungsi isNext(d1, d2)
          putStrLn $ "isNext(d1, d2) = " ++ show (isNext d1 d2)
          -- memanggil fungsi isBeforeN(d1, d2, n)
          putStrLn $ "isBeforeN(d1, d2, n) = " ++ show (isBeforeN d1 d2 n)
          -- memanggil fungsi isNextN(d1, d2, n)
          putStrLn $ "isNextN(d1, d2, n) = " ++ show (isNextN d1 d2 n)