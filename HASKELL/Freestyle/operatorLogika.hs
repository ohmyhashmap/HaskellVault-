main :: IO()
main = do
    let nilai :: Bool
        nilai = True
    let nilai2 :: Bool
        nilai2 = False
    let nilai3 :: Bool
        nilai3 = True
    let nilai4 :: Bool
        nilai4 = False
    -- operator logika and
    let result = nilai && nilai2
    let result2 = nilai && nilai3
    let result3 = nilai2 && nilai4
    print result
    print result2
    print result3
    -- operator logika or
    let result = nilai || nilai2
    let result2 = nilai || nilai3
    let result3 = nilai2 || nilai4
    print result
    print result2
    print result3
    -- operator logika not
    let result = not nilai
    let result2 = not nilai2
    print result
    print result2
