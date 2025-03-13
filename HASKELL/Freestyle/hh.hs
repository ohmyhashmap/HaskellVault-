printHelloWorld :: Int -> IO ()
printHelloWorld 0 = return () -- mengembalikan void
printHelloWorld n = do
    putStrLn "Hello World"
    printHelloWorld (n - 1)

main :: IO()
main = do
    n <- readLn :: IO Int

    -- Print "Hello World" on a new line 'n' times.
    printHelloWorld n

