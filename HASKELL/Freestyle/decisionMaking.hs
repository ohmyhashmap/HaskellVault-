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
    -- if-else statements
    if nilai && nilai3
        then putStrLn "Memilih Jalur 1a"
        else putStrLn "Memilih Jalur 1b"
    -- if-elseif-else statements
    if nilai && not nilai3 -- true & neg (true) = false lewat
        then putStrLn "Memilih Jalur 2a"
    else if nilai && nilai3 -- true & true = true = printed
        then putStrLn "Memilih Jalur 2b"
    else putStrLn "Memilih Jalur 2c"