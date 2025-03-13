-- Fungsi untuk mentranspose matriks
transpose :: [[a]] -> [[a]]
transpose [] = []  -- Basis: matriks kosong
transpose ([]:_) = []  -- Basis: jika ada baris kosong
transpose matrix = (map head matrix) : transpose (map tail matrix)

main :: IO ()
main = do
    let matrixA = [[1, 2, 3], [4, 5, 6]]
    let transposedMatrix = transpose matrixA
    print transposedMatrix