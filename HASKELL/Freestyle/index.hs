main :: IO ()
main = do
    putStrLn "Masukkan sesuatu:"
    input <- getLine
    putStrLn ("Anda memasukkan: " ++ input)