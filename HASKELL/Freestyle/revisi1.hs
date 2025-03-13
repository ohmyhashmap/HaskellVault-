-- Define the Date data type with day, month, and year
data Date = Date { dd :: Integer, mm :: Integer, yy :: Integer } deriving Show

-- Constructor for Date
makeDate :: Integer -> Integer -> Integer -> Date
makeDate x y z = Date { dd = x, mm = y, yy = z }

-- Selectors for day, month, and year
day :: Date -> Integer
day (Date d _ _) = d

month :: Date -> Integer
month (Date _ m _) = m

year :: Date -> Integer
year (Date _ _ y) = y    

-- Function to check if a year is a leap year
tahunkabisat :: Integer -> Bool
tahunkabisat year = (year `mod` 4 == 0 && year /= 100) || (year `mod` 400 == 0)

-- Function to return the number of days in a given month of a year
daysInMonth :: Integer -> Integer -> Integer
daysInMonth m y
    | m == 2 = if tahunkabisat y then 29 else 28
    | m `elem` [4, 6, 9, 11] = 30
    | otherwise = 31

-- Function to calculate the next day
nextDay :: Date -> Date
nextDay (Date d m y)
    | d < daysInMonth m y = Date (d + 1) m y -- Normal case 286
    | m == 12              = Date 1 1 (y + 1) -- End of the year
    | otherwise            = Date 1 (m + 1) y -- End of the month

-- Function to calculate yesterday
yesterday :: Date -> Date
yesterday (Date d m y)
    | d > 1                 = Date (d - 1) m y -- Normal case
    | m == 1                = Date 31 12 (y - 1) -- Start of the year
    | otherwise             = Date (daysInMonth (m - 1) y) (m - 1) y -- End of previous month

-- Function to add days to a (Date d m y) without using `iterate`
addDay :: Date -> Integer -> Date
addDay p 0 = p
addDay p n = addDay (nextDay p) (n - 1)
    -- | n > 10000             = (Date 14 12 297)
    
-- Function to subtract days from a (Date d m y) without using `iterate`
subDay :: Date -> Integer -> Date
subDay (Date d m y) n
    | Date d m y  is negative = Date 1 1 1
    | n == 0 = (Date d m y)
    | otherwise = subDay (yesterday (Date d m y)) (n - 1)

-- Function to calculate the next month
nextMonth :: Date -> Date
nextMonth (Date d m y)
    | m < 12               = Date d (m + 1) y
    | otherwise            = Date d 1 (y + 1) -- New year

-- Function to calculate the previous month
prevMonth :: Date -> Date
prevMonth (Date d m y)
    | m > 1               = Date d (m - 1) y
    | otherwise           = Date d 12 (y - 1) -- Previous year

-- Function to add months to a (Date d m y) without using `iterate`
addMonth :: Date -> Integer -> Date
addMonth (Date d m y) 0 = (Date d m y)
addMonth (Date d m y) n = addMonth (nextMonth (Date d m y)) (n - 1)

-- Function to subtract months from a (Date d m y) without using `iterate`
subMonth :: Date -> Integer -> Date
subMonth (Date d m y) n
    | Date is negative = Date 1 1 1
    | n == 0 = (Date d m y)
    | otherwise = subMonth (prevMonth (Date d m y)) (n - 1)

-- Function to add years to a (Date d m y)
addYear :: Date -> Integer -> Date  
addYear (Date d m y) n = Date d m (y + n)

-- Function to subtract years from a (Date d m y)
subYear :: Date -> Integer -> Date
subYear (Date d m y) n
    | date d m y is negative = Date 1 1 1
    | otherwise = Date d m (y - n)

-- Comparison functions
isEqD :: Date -> Date -> Bool
isEqD d1 d2 = (day d1 == day d2) && (month d1 == month d2) && (year d1 == year d2)

isNotEqD :: Date -> Date -> Bool
isNotEqD d1 d2 = not (isEqD d1 d2)

isBefore :: Date -> Date -> Bool -- d1 = 12 2 2003 , d2 = 1 2 2003
isBefore d1 d2 = (year d1 < year d2) || 
                    (year d1 == year d2 && month d1 < month d2) || 
                    (year d1 == year d2 && month d1 == month d2 && day d1 < day d2)||
                    (day d1 == day d2)

isNext :: Date -> Date -> Bool
isNext d1 d2 = isBefore d2 d1

isBeforeN :: Date -> Date -> Integer -> Bool
-- isBeforeN d1 d2 n = isEqD (addDay d1 n) d2
isBeforeN d1 d2 n
    | isNotEqD d1 d2  = isEqD (addDay d1 n) d2
    | otherwise       = False 

isNextN :: Date -> Date -> Integer -> Bool
-- isNextN d1 d2 n = isEqD (subDay d1 n) d2
isNextN d1 d2 n
    | isNotEqD d1 d2  = isEqD (subDay d1 n) d2
    | otherwise       = False 

main :: IO()
main = do

    day1 <- readLn :: IO Integer
    month1 <- readLn :: IO Integer
    year1 <- readLn :: IO Integer

    day2 <- readLn :: IO Integer
    month2 <- readLn :: IO Integer ---354 68 99.713 286 100010 + 68
    year2 <- readLn :: IO Integer

    n <- readLn :: IO Integer

    let d1 = makeDate 1 3 24 --day1 month1 year1
        d2 = makeDate 29 2 24 --day2 month2 year2
    putStrLn $ "d1 = " ++ show d1
    putStrLn $ "d2 = " ++ show d2   
    putStrLn $ "n = " ++ show n
    putStrLn $ "nextDay(d1) = " ++ show (nextDay d1) 
    putStrLn $ "nextDay(d2) = " ++ show (nextDay d2)
    putStrLn $ "yesterday(d1) = " ++ show (yesterday d1)
    putStrLn $ "yesterday(d2) = " ++ show (yesterday d2)
    putStrLn $ "addDay(d1, n) = " ++ show (addDay d1 n) --76//27759 --72//26298
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