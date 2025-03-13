-- Perkenalkan Kami
-- NAMA \\ NIM
-- Christian Sitohang \\ 11S24039
-- Immanuel Lumbantobing \\ 11S24035
-- Feny Pasaribu \\ 11S24026
-- Cahaya Silaen \\ 11S24017)
-- Miftahul siregar \\ 11S24007
-- proyek Haskell dengan banyak file modul membuat banyak modul 

-- Tipe Bentukan (Objek) yang dibuat Buku, Siswa dan Ruangan. (selesai)
-- list of list

import qualified Mahasiswa as M
import qualified Buku as B
import qualified Ruangan as R
import qualified Keuangan as K

doubleMe x = x + x
doubleUs x y = x*2 + y*2
doubleSmallNumber x = (if x > 100 then x else x*2) + 1

main :: IO ()
main = do
  B.main  -- Memanggil main dari modul Buku
  M.main  -- Memanggil main dari modul Mahasiswa
  R.main  -- Memanggil main dari modul Ruangan
  K.main  -- Memanggil main dari modul Keuangan
  
-- List di dalam List (nested list)
