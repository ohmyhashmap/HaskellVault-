main :: IO()
main = do
    -- 1) Data Number
    -- mendeklarasikan bilangan bulat
    let dataInt = 42
    
    -- mendeklarasikan bilangan riil
    let dataDouble = 3.14

    -- menampilkan number
    print dataInt
    print dataDouble
    -- 2) Data Character
    -- mendeklarasikan character
    let dataChar = 'A'
    -- menampilkan character
    print dataChar
    -- 3) Data String
    -- mendeklarasikan string
    let dataStr = "Belajar bahasa pemrograman Haskell bersama"
    let dataStr2 = "Abdullah Ubaid"
    -- menampilkan string
    putStrLn dataStr
    putStrLn dataStr2
    -- 3) Data Boolean
    -- mendeklarasikan boolean
    let dataBool = True
    -- menampilkan boolean
    print dataBool
    -- 3) Data List
    -- mendeklarasikan list
    let myList = [6, 9, 1999]
    -- menampilkan list
    print myList
    -- 4) Data Tuple
    -- mendeklarasikan list
    let myTuple = (3, '-', 5.0, False)
    -- menampilkan tuple
    print myTuple