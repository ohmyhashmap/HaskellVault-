module SortList where
    -- SORT LIST                                     sortList(l)
    -- DEFINISI DAN SPESIFIKASI
    sortList :: [Int] -> [Int]
    {- sortList(l) mengembalikan hasil pengurutan list l hingga elemen-elemennya
    terurut membesar. Prekondisi: l tidak kosong dan semua elemennya unik. -}
    isOneElmt :: [Int] -> Bool 
    {- isOneElmt(l) mengembalikan true jika hanya ada 1 elemen pada list l. -}
    isOneElmt l = length l == 1
    -- KONSTRUKTOR
    konso :: Int -> [Int] -> [Int]
    {- konso e li menghasilkan sebuah list of integer dari li dan e dengan e sebagai elemen pertama. -}
    konso e li = [e] ++ li
    -- Soal 4.
    -- REALISASI
    sortList l
        | isOneElmt l = [head l]                                                                                   -- Basis
        | otherwise = if head l < head (sortList (tail l)) then konso (head l) (sortList (tail l))                 -- Rekurens
            else konso (head (sortList (tail l))) (sortList (konso (head l) (tail (sortList (tail l)))))
            
    -- CONTOH APLIKASI
    -- sortList [7,3,9,13,15,31,19]
    -- [3,7,9,13,15,19,31]
    -- sortList [6,15,4,9]
    -- [4,6,9,15]
    -- sortList [9,15,27]
    -- [9,15,27]