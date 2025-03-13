nElement :: [a] -> Int
nElement [] = 0 -- Basis, saat list kosong hentikan proses
nElement (element:listsisa) =  nElement listsisa + 1 -- Rekursi dengan memotong elemen pertama list
-- ANALISIS KODE
-- disini kita lihat element adalah himpunan atau sets yang kosong maka hasil mengembalikan integer 0 
-- element adalah unsur pertama himpunan dan listsisa adalah sisa unsur selain unsur pertama
-- lalu rekursi dengan setiap kali fungsi melakukan pemanggilan pada fungsinya sampai listsisa = []
-- dimana itu adalah himpunan kosong 
sumElement :: Num a => [a] -> a
sumElement [] = 0 -- Basis, saat list kosong hentikan proses
sumElement (element:listSisa) = element + sumElement listSisa -- Rekursi untuk menjumlahkan elemen
-- ANALISIS KODE
-- disini element adalah variabel yang memuat elemen pertama dalam list berupa kumpulan elemen tipe data integer
-- Jadi rekursi dengan elemen pertama list + sisa elemen di setiap pemanggilan 

main = do
    let lst = [1, 2, 3, 4, 5] 
        totalElement = nElement lst -- 1 + ( 1 + ( 1 + ( 1 + (1 ) ) ) )
        jumlahElement = sumElement lst -- 1 + ( 2 + ( 3 + ( 4 + (5 ) ) ) )
    putStrLn ("Total Element: " ++ show totalElement)
    putStrLn ("Jumlah Element: " ++ show jumlahElement)