fungsiTambah :: Int -> Int -> Int
fungsiTambah x y = x + y  

addInt :: Int -> Int -> Int
addInt x y = x + y\



main = do 
    let num = addInt 2 12
        halo = putStrLn "Halo Nama saya \"Christian Alberto Sitohang\""
    putStrLn $ show halo
    putStrLn $ show (addInt 2 5) ++ "\n"++ show num