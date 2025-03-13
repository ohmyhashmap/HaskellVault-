-- Data Type for Time
data Time = Time { hh :: Integer, mm :: Integer, ss :: Integer } deriving Show

-- Constructor: makeTime(hh, mm, ss), creates a Time from hours, minutes, and seconds.
-- makeTime(hh, mm, ss), untuk membentuk Time, dari jam hh, menit mm, dan detik ss.
makeTime :: Integer -> Integer -> Integer -> Time
makeTime a b c = Time { hh = a, mm = b, ss = c }

-- Selectors
hour :: Time -> Integer
hour t = hh t   -- Returns the hour (hh) from Time t. hour(t), untuk mengembalikan nilai jam (hh) dari Time t
minute :: Time-> Integer
minute t = mm t -- Returns the minute (mm) from Time t. minute(t), untuk mengembalikan nilai menit (mm) dari Time t.
second :: Time -> Integer
second t = ss t -- Returns the second (ss) from Time t. second(t), untuk mengembalikan nilai detik (ss) dari Time t.

-- Operator Functions

-- nextSecond(t): calculates the next second from Time t.
-- nextSecond(t), untuk menentukan waktu berikutnya dari Time t (Menambah 1 detik).
nextSecond :: Time -> Time
nextSecond t = Time 
    { 
        hh = if hour t == 23 && minute t == 59 && second t == 59 then 0
            else if minute t == 59 && second t == 59 then (hour t `mod` 24) + 1 -- 544354 mod 24
            else (hour t + ((minute t + (second t `div` 60)) `div` 60)) `mod` 24,
        mm = if minute t == 59 && second t == 59 then 0
            else if second t == 59 then (minute t `mod` 60) + 1
            else (minute t + (second t `div` 60)) `mod` 60,
        ss = if second t == 59 then 0
            else if second t > 59 then second t `mod` 60 + 1 -- 90 mod      
            else second t + 1
    }

-- previousSecond(t): calculates the previous second from Time t. 
-- previousSecond(t), untuk menentukan waktu sebelumnya dari Time t (Mengurangi 1 detik).
prevSecond :: Time -> Time
prevSecond t = Time 
    {   
        hh = if hour t == 0 && minute t == 0 && second t == 0 then 23
            else if hour t > 0 && minute t == 0 && second t == 0 then (hour t `mod` 24) - 1 
            else if hour t > 0 && minute t > 0 && second t > 0 then (hour t + (minute t + ((second t - 1) `div` 60)) `div` 60) `mod` 24 
            else hour t,
        mm = if minute t == 0 && second t == 0 then 59
            else if minute t > 0 && second t == 0 then minute t `mod` 60 - 1
            else if minute t > 0 && second t > 0 then (minute t + (second t - 1) `div` 60) `mod` 60
            else minute t,
        ss = if second t == 0 then 59
            else if second t > 0 then (second t - 1) `mod` 60
            else second t
    }

-- addSeconds(t, n): Adds n seconds to Time t.
-- addSeconds(t, n), untuk menambahkan detik sebanyak n pada Time t.
addSeconds :: Time -> Integer -> Time
addSeconds t 0 = t
addSeconds t n = addSeconds (nextSecond t) (n - 1)

-- subSeconds(t, n): Subtracts n seconds from Time t.
-- subSeconds(t, n), untuk mengurangi detik sebanyak n pada Time t.
subSeconds :: Time -> Integer -> Time
subSeconds t 0 = t
subSeconds t n = subSeconds (prevSecond t) (n - 1)

-- nextMinute(t) calculates the next minute from Time t
-- nextMinute(t), untuk menentukan waktu berikutnya dari Time t (Menambah satu menit).
nextMinute :: Time -> Time -- 12 59 23
nextMinute t = Time 
    {
        hh = if hour t > 0 && minute t == 59 then (hour t `mod` 24) + 1
            else if hour t > 0 && minute t > 59 then (hour t + (minute t + (second t `div` 60))`div` 60) `mod` 24 + 1
            else hour t,
        mm = if minute t > 0 then (minute t + (second t `div` 60 ))`mod` 60 + 1
            else minute t,
        ss = if  second t >= 0 then second t `mod` 60
            else second t
    }

-- previousMinute(t) calculates the previous minute from Time t
-- previousMinute(t), untuk menentukan waktu sebelumnya dari Time t (Mengurangi satu menit).
prevMinute :: Time -> Time
prevMinute t = Time 
    {
        hh = if hour t == 0 && minute t == 0 then 23 -- 13 0 0
            else if hour t > 0 && minute t > 0 then (hour t + (minute t + (second t `div` 60))`div` 60) `mod` 24 - 1
            else hour t,
        mm = if minute t == 0 then 59
            else if minute t > 0 then (minute t + (second t `div` 60)) `mod` 60 + 1
            else minute t,
        ss = if second t > 0 then second t `mod` 60
            else second t
    }

-- addMinutes(t, n) Adds n minutes to Time t
-- addMinutes(t, n), untuk menambahkan menit sebanyak n pada Time t.
addMinutes :: Time -> Integer -> Time
addMinutes t 0 = t
addMinutes t n = addMinutes (nextMinute t) (n - 1)

-- subMinutes(t, n) Subtracts n minutes from Time t
-- subMinutes(t, n), untuk mengurangi menit sebanyak n pada Time t.
subMinutes :: Time -> Integer -> Time
subMinutes t 0 = t
subMinutes t n = subMinutes( prevMinute t) (n - 1)

-- nextHour(t) calculates the next hour from Time t
-- nextHour(t), untuk menentukan waktu 1 jam berikutnya dari Time t.
nextHour :: Time -> Time
nextHour t = Time {}
-- previousHour(t) calculates the next hour from Time t
-- previousHour(t), untuk menentukan waktu 1 jam sebelumnya dari Time t.


-- addHours(t, n) Adds n Hours to Time t
-- addHours(t, n), untuk menambahkan jam sebanyak n pada Time t.
addHours :: Time -> Integer -> Time
addHours t 0 = t
addHours t n = addHours(nextHour t)(n-1) 

-- subHours(t, n) Subtracts n Hours from Time t
-- subHours(t, n), untuk mengurangi jam sebanyak n pada Time t.

-- timeToSeconds(t): Converts Time to total seconds.
-- timeToSeconds(t): Fungsi ini mengonversi tipe bentukan Time (jam, menit, detik) menjadi total detik.

-- isEqT(t1, t2) Compare if two Time values are the same
-- isEqT(t1, t2), untuk membandingkan, apakah kedua Time sama.

-- isNotEqT(t1, t2) Compare if two Time values are not the same.
-- isNotEqT(t1, t2), untuk membandingkan, apakah kedua Time tidak sama.

-- isBeforeT(t1, t2) returns True if t1 is before t2
-- isBeforeT(t1, t2), mengembalikan nilai benar jika Time t1 sebelum Time t2.

-- isAfterT(t1, t2) returns True if t1 is after t2
-- isAfterT(t1, t2), mengembalikan nilai benar jika Time t1 sesudah Time t2.

-- isBeforeNSeconds(t1, t2, n) returns True if t1 is before t2 by n seconds
-- isBeforeNSeconds(t1, t2, n), mengembalikan nilai benar jika Time t1 sebelum Time t2 sebanyak n detik.

-- isAfterNSeconds(t1, t2, n) returns True if t1 is after t2 by n seconds
-- isAfterNSeconds(t1, t2, n), mengembalikan nilai benar jika Time t1 sesudah Time t2 sebanyak n detik


timeToSeconds :: Time -> Integer -> Integer
timeToSeconds t n = hour t * 3600 + minute t * 60 + second t + n

-- secondsToTime(s): Converts total seconds to Time.
-- secondsToTime(s): Fungsi ini mengonversi total detik s menjadi tipe bentukan Time.
secondsToTime :: Integer -> Time
secondsToTime s = makeTime ((s `div` 3600) `mod` 24) ((s `mod` 3600) `div` 60) (s `mod` 60)

-- Main Program
main = do
    hours1 <- readLn :: IO Integer
    minutes1 <- readLn :: IO Integer
    seconds1 <- readLn :: IO Integer
    hours2 <- readLn :: IO Integer
    minutes2 <- readLn :: IO Integer
    seconds2 <- readLn :: IO Integer
    n <- readLn :: IO Integer

    let t1 = makeTime hours1 minutes1 seconds1
        t2 = makeTime hours2 minutes2 seconds2

    putStrLn $ "t1 = " ++ show t1
    putStrLn $ "t2 = " ++ show t2
    putStrLn $ "n = " ++ show n
    putStrLn $ "nextSecond(t1) = " ++ show (nextSecond t1)
    putStrLn $ "nextSecond(t2) = " ++ show (nextSecond t2)
    putStrLn $ "previousSecond(t1) = " ++ show (prevSecond t1)
    putStrLn $ "previousSecond(t2) = " ++ show (prevSecond t2)
    putStrLn $ "addSeconds(t1, n) = " ++ show (addSeconds t1 n)
    putStrLn $ "addSeconds(t2, n) = " ++ show (addSeconds t2 n)
    putStrLn $ "subSeconds(t1, n) = " ++ show (subSeconds t1 n)
    putStrLn $ "subSeconds(t2, n) = " ++ show (subSeconds t2 n)
    putStrLn $ "nextMinute(t1) = " ++ show (nextMinute t1)
    putStrLn $ "nextMinute(t2) = " ++ show (nextMinute t2)
    putStrLn $ "previousMinute(t1) = " ++ show (prevMinute t1)
    putStrLn $ "previousMinute(t2) = " ++ show (prevMinute t2)
    putStrLn $ "addMinutes(t1, n) = " ++ show (addMinutes t1 n)
    putStrLn $ "addMinutes(t2, n) = " ++ show (addMinutes t2 n)
    putStrLn $ "subMinutes(t1, n) = " ++ show (subMinutes t1 n)
    putStrLn $ "subMinutes(t2, n) = " ++ show (subMinutes t2 n)
    putStrLn $ "timeToSeconds(t1) = " ++ show (timeToSeconds t1 0)
    putStrLn $ "timeToSeconds(t2) = " ++ show (timeToSeconds t2 0)
    putStrLn $ "[t1] secondsToTime(" ++ show (timeToSeconds t1 n) ++ ") = " ++ show (secondsToTime (timeToSeconds t1 n))
    putStrLn $ "[t2] secondsToTime(" ++ show (timeToSeconds t2 n) ++ ") = " ++ show (secondsToTime (timeToSeconds t2 n))