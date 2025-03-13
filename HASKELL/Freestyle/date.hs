data Date = Date { dd :: Integer, mm :: Integer, yy :: Integer} deriving Show

--Konstruktor
makeDate :: Integer -> Integer -> Integer -> Date
makeDate x y z = Date { dd = x, mm = y, yy = z}

--Selektor
day :: Date -> Integer
day Date = dd Date

month :: Date -> Integer
month Date = mm Date

year :: Date -> Integer
year Date = yy Date    

tahunkabisat :: Integer -> Bool
tahunkabisat year = (year `mod` 4 == 0 && year /= 100) || (year `mod` 400 == 0)

daysInMonth :: Integer -> Integer -> Integer
daysInMonth month year
    | month == 2 = if tahunkabisat year then 29 else 28
    | month `elem` [4, 6, 9, 11] = 30
    | otherwise = 31

nextDay :: Date -> Date
nextDay Date d m y
    | d < daysInMonth m y = Date (d + 1) m y -- 21 7 2006 -> 22 7 2006, 1 12 2006 -> 2 12 2006
    | m == 12              = Date 1 1 (y + 1) --  31 12 9008 -> 1 1 9009
    | otherwise            = Date 1 (m + 1) y -- 31 3 2005 -> 1;4(3+1);2005 

yesterday :: Date -> Date
yesterday Date d m y
    | d < daysInMonth m y = Date (d - 1) m y -- 21;7;2006 -> 20;7;2006, 1;12;2006 -> 30 11 2006
    | m == 12              = Date 1 1 (y - 1) --  31;12;9008 -> 30;12;9008
    | otherwise            = Date 1 (m - 1) y -- 31 3 2005 -> 30;3;2005 

addDay :: Date -> Integer -> Date
addDay Date n 
    | m == 12 && d + n > daysInMonth m y     = Date ((d + n) - daysInMonth) 1 (y+1)  --  31 12 9008 -> 1 1 9009
    | d + n < daysInMonth m y                = Date (d + n) m y -- 21 7 2006  + 2 hari -> 24 7 2006
    | d + n > daysInMonth m y                = Date ((d + n) - daysInMonth) (m + 1) y -- 21 7 2006 + 23 => 44-31= 13 -> 13 7 2006
    | otherwise                              = Date 1 (m + 1) y -- 31 3 2005 -> 1;4(3+1);2005 

subDay :: Date -> Integer -> Date
subDay Date n 
    | d < daysInMonth m y = Date (d + 1) m y -- 21 7 2006 -> 22 7 2006, 1 12 2006 -> 2 12 2006
    | m == 12              = Date 1 1 (y + 1) --  31 12 9008 -> 1 1 9009
    | otherwise            = Date 1 (m + 1) y -- 31 3 2005 -> 1;4(3+1);2005 

nextMonth :: Date -> Date
nextMonth Date d m y
    | m < 12               = Date d (m + 1) y
    | otherwise            = Date d (1) (y+1) -- 31 3 2005 -> 1;4(3+1);2005 

prevMonth :: Date -> Date
prevMonth Date d m y
    | m > 1               = Date d (m - 1) y
    | otherwise            = Date d (12) (y-1) -- 31 3 2005 -> 1;4(3+1);2005 

addMonth :: Date -> Date
addMonth Date d m y
    | m < 12               = Date d (m + 1) y
    | otherwise            = Date d (1) (y+1) -- 31 3 2005 -> 1;4(3+1);2005 
main :: IO()
main = do
    day1 <- readLn :: Integer
    month1 <- readLn :: Integer
    year1 <- readLn :: Integer
    day2 <- readLn :: Integer
    month2 <- readLn :: Integer
    year2 <- readLn :: Integer

    let d1 = makeDate day1 month1 year1
        d2 = makeDate day2 month2 year2
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