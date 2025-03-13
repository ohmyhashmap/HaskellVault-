-- fibonnaci :: Integer -> Integer -> Integer -> Integer
fibonnaci a _ 0 = a  -- Base case: return the first number when n = 0
fibonnaci _ b 1 = b  -- Base case: return the second number when n = 1
fibonnaci a b n = fibonnaci a b (n-1) + fibonnaci a b (n-2)  -- Recursive case

main :: IO ()
main = do
    input1 <- getLine
    input2 <- getLine
    input3 <- getLine
    let a = read input1 :: Integer
    let b = read input2 :: Integer
    let n = read input3 :: Integer
    putStrLn $ "The Fibonacci number at position " ++ show n ++ " is " ++ show (fibonnaci a b n)