--CHRISTIAN ALBERTO SITOHANG 11S24039
main :: IO()
main = do
    let nilaiInt :: Int
        nilaiInt = 20
    let nilaiInt2 :: Int
        nilaiInt2 = 20
-- mengecek kesamaan data
    let hasil = nilaiInt == nilaiInt2
    print hasil
-- mengecek ketidak samaan data
    let hasil = nilaiInt /= nilaiInt2
    print hasil
-- mengecek apakah data lebih besar
    let hasil = nilaiInt > nilaiInt2
    print hasil
-- mengecek apakah data lebih besar dan sama dengan
    let hasil = nilaiInt >= nilaiInt2
-- mengecek apakah data lebih kecil
    let hasil = nilaiInt < nilaiInt2
    print hasil
-- mengecek apakah data lebih kecil dan sama dengan
    let hasil = nilaiInt <= nilaiInt2
    print hasil