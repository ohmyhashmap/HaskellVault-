-- Palindrome
isPalindrome :: [String] -> Bool
isPalindrome str = str ==  reversing str

-- Fungsi untuk membalik string secara manual
reversing :: [a] -> [a]
reversing [] = []
reversing (x:xs) = reversing xs ++ [x] 

main :: IO ()
main = do
    input <- getLine
    putStrLn ("isPalindrome ("++ show input ++ ") = " ++ reversing input)