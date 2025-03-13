-- Define the Date data type with day, month, and year
data Date = Date { dd :: Integer, mm :: Integer, yy :: Integer } deriving (Show, Eq)
--Adding Eq to the deriving clause allows Haskell to automatically generate an instance of the Eq typeclass,
-- so you can use == and /= to compare Date values. After making this change, isNext should work without any issues.

-- Constructor for Date
makeDate :: Integer -> Integer -> Integer -> Date
makeDate x y z = Date { dd = x, mm = y, yy = z }

-- Selectors for day, month, and year
day :: Date -> Integer
day date = dd date

month :: Date -> Integer
month date = mm date

year :: Date -> Integer
year date = yy date    

-- Function to check if a year is a leap year
tahunkabisat :: Integer -> Bool
tahunkabisat y = (y `mod` 4 == 0 && y `mod` 100 /= 0) || (y `mod` 400 == 0)

-- Function to return the number of days in a given month of a year
daysInMonth :: Integer -> Integer -> Integer
daysInMonth m y
    | m == 2 = if tahunkabisat y then 29 else 28
    | m `elem` [4, 6, 9, 11] = 30
    | otherwise = 31

-- Adjusts the date to default if the year goes below 1
defaultIfBelowZero :: Date -> Date
defaultIfBelowZero (Date d m y)
    | y < 1     = Date 1 1 1
    | otherwise = Date d m y

-- Function to calculate the next day
nextDay :: Date -> Date
nextDay date
    | day date < daysInMonth (month date) (year date) = Date (day date + 1) (month date) (year date)
    | month date == 12 = Date 1 1 (year date + 1)
    | otherwise = Date 1 (month date + 1) (year date)

-- Function to calculate yesterday
yesterday :: Date -> Date
yesterday date
    | day date > 1 = Date (day date - 1) (month date) (year date)
    | month date == 1 = Date 31 12 (year date - 1)
    | otherwise = Date (daysInMonth (month date - 1) (year date)) (month date - 1) (year date)

-- Function to add days to a Date without using iterate
addDay :: Date -> Integer -> Date
addDay date 0 = date
addDay date n = addDay (nextDay date) (n - 1)

-- Function to subtract days from a Date without using iterate\
-- Revisi
-- subDay :: Date -> Integer -> Date
-- subDay date 0 = date
-- subDay date n = subDay (yesterday date) (n - 1)
-- teruntuk tahun di gregorian date itu tidak ada minus template datenya jadi dikurangi karena lebih kecil dari Date { 1, 1, 1 } 
-- maka teruntuk Date { 1, 1, 1 }

-- Subtract days with default handling
subDay :: Date -> Integer -> Date
subDay date 0 = defaultIfBelowZero date
subDay date n = subDay (yesterday date) (n - 1)

-- Function to calculate the next month
nextMonth :: Date -> Date
nextMonth (Date d m y)
    | m == 12 = Date d 1 (y + 1)
    | otherwise = Date d (m + 1) y

-- Function to calculate the previous month
prevMonth :: Date -> Date
prevMonth (Date d m y)
    | m == 1 = Date d 12 (y - 1)
    | otherwise = Date d (m - 1) y

-- Function to add months to a Date without using iterate
addMonth :: Date -> Integer -> Date
addMonth date 0 = date
addMonth (Date d m y) n = addMonth (nextMonth (Date d m y)) (n - 1)

-- Function to subtract months from a Date without using iterate
-- subMonth :: Date -> Integer -> Date
-- subMonth date 0 = date
-- subMonth (Date d m y) n = subMonth (prevMonth (Date d m y)) (n - 1) 
-- teruntuk tahun di gregorian date itu tidak ada minus template datenya jadi dikurangi karena lebih kecil dari Date { 1, 1, 1 } 
-- maka teruntuk Date { 1, 1, 1 }

-- Subtract months with default handling
subMonth :: Date -> Integer -> Date
subMonth date 0 = defaultIfBelowZero date
subMonth date n = subMonth (prevMonth date) (n - 1)

-- Function to add years to a Date
addYear :: Date -> Integer -> Date  
addYear (Date d m y) n = Date d m (y + n)

-- Function to subtract years from a Date
-- subYear :: Date -> Integer -> Date
-- subYear (Date d m y) n = Date d m (y - n) 
-- teruntuk tahun di gregorian date itu tidak ada minus template datenya jadi dikurangi karena lebih kecil dari Date { 1, 1, 1 } 
-- maka teruntuk Date { 1, 1, 1 }

-- Subtract years with default handling
subYear :: Date -> Integer -> Date
subYear (Date d m y) n = defaultIfBelowZero (Date d m (y - n))

-- Comparison functions
isEqD :: Date -> Date -> Bool
isEqD (Date d1 m1 y1) (Date d2 m2 y2) = d1 == d2 && m1 == m2 && y1 == y2

isNotEqD :: Date -> Date -> Bool
isNotEqD d1 d2 = not (isEqD d1 d2)

isBefore :: Date -> Date -> Bool
isBefore (Date d1 m1 y1) (Date d2 m2 y2) = (y1, m1, d1) < (y2, m2, d2) -- sebenarnya fungsi ini berjalan dengan baik 
            --namun di TC (Type Case) tidak dijelaskan dengan baik bahwa tanggal pertama itu dicek hanya
            -- apakah tanggal pertama sebelum tanggal kedua sebanyak 1 hari saja  bukan terserah sebanyak apa gitu //lihat TC 

isNext :: Date -> Date -> Bool
isNext (Date d1 m1 y1) (Date d2 m2 y2) = (y1, m1, d1) > (y2, m2, d2) -- sebenarnya fungsi ini berjalan dengan baik 
            --namun di TC (Type Case) tidak dijelaskan dengan baik bahwa tanggal pertama itu dicek hanya
            -- apakah tanggal pertama sesudah tanggal kedua sebanyak 1 hari saja bukan terserah sebanyak apa gitu //lihat TC 

isBeforeN :: Date -> Date -> Integer -> Bool
isBeforeN d1 d2 n = addDay d1 n == d2

isNextN :: Date -> Date -> Integer -> Bool
isNextN d1 d2 n = subDay d2 n == d1 --kesalahan  disini harusnya dicek tanggal pertama harus di kurangi sebanyak n kali
                                    -- dan diperiksa apakah benar sama nilai nya dengan tanggal kedua

-- Main function to test
main :: IO()
main = do

    day1 <- readLn :: IO Integer
    month1 <- readLn :: IO Integer
    year1 <- readLn :: IO Integer

    day2 <- readLn :: IO Integer
    month2 <- readLn :: IO Integer
    year2 <- readLn :: IO Integer

    n <- readLn :: IO Integer
    let d1 = makeDate day1 month1 year1
        d2 = makeDate day2 month2 year2
    -- Displaying results
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