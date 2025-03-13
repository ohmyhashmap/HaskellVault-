-- Fungsi ini menjumlahkan tiga elemen pertama dan menjumlahkan sisanya secara terpisah
sumFirstTwo :: Num a => [a] -> a
sumFirstTwo (element:element2:listSisa) = element + element2 + sum listSisa
sumFirstTwo _ = 0 -- Jika list memiliki kurang dari 3 elemen, kembalikan 0


main = do
    let lst1 = [1..100  ]
    let lst2 = [10, 20] -- suatu kondisi dimana list punya 2 elemen saja dan tidak ada elemen ketiga di handle 
    putStrLn ("Sum of first two elements plus the rest in lst1: " ++ show (sumFirstTwo lst1))
    putStrLn ("Sum of first two elements plus the rest in lst2: " ++ show (sumFirstTwo lst2))   