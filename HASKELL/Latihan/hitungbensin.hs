function :: Int -> Int
function x = 
    if x `mod` 2 == 0
        then (x `div` 2)
        else (3*x + 1)

-- loop :: [a] -> Int
-- loop [] = 0
-- loop (element:listsisa) = loop listsisa + 1

hitungbensin :: Int -> Int -> Int
hitungbensin x y =
    if x < y
        then 0
        else function x + hitungbensin x (y-1)

main = do
    a <- readLn :: IO Int
    b <- readLn :: IO Int

    let urutan = hitungbensin a b 
    print (urutan)