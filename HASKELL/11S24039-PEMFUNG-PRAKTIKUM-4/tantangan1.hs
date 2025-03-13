subfilum :: Int -> String
subfilum p1 =
    if p1 == 1
        then "Vertebrata"
    else if p1 == 2
        then "Invertebrata"
    else "Tidak Ada" -- Tambahkan default case

ciri :: Int -> Int -> String
ciri p1 p2 =
    if p1 == 1
        then 
            if p2 == 1
                then "Warm-Blooded"
            else if p2 == 2 
                then "Cold-Blooded"
            else "Tidak ada" 
    else if p1 == 2
            then
                if p2 == 1
                    then "With Jointed Legs"
                else if p2 == 2
                    then "Without Legs"
                else"Tidak ada"
    else "Tidak Ada" -- Tambahkan default case

kelas :: Int -> Int -> Int -> String
kelas p1 p2 p3 =
    if p1 == 1
        then
            if p2 == 1
                then
                    if p3 == 1
                        then "Mammals"
                    else if p3 == 2
                        then "Birds"
                    else "Tidak ada" --Tambahkan default case
            else if p2 == 2
                    then 
                        if p3 == 1
                            then "Fish"
                        else if p3 == 2
                            then "Reptiles"
                        else if p3 == 3
                            then "Amphibians"
                        else "Tidak Ada" --Tambahkan default case
                    else "Tidak Ada"
    else if p1 == 2
            then
                if p2 == 1
                    then
                        if p3 == 1
                            then "With 3 pairs of legs"
                        else if p3 == 2
                            then "With more than 3 pairs of legs"
                        else "Tidak Ada"
                else if p2 == 2
                        then
                            if p3 == 1
                                then "Worm-like" 
                            else if p3 == 2
                                then "Not-worm like"
                            else "Tidak Ada"    
                else "Tidak Ada"
    else "Tidak Ada"

hewan :: Int -> Int -> Int -> String
hewan p1 p2 p3 =
    if p1 == 1
        then
            if p2 == 1
                then
                    if p3 == 1
                        then "Bear, Tiger, Whale"
                    else if p3 == 2
                        then "Ostrich, Peacock, Eagle"
                    else "Tidak Ada"
                else if p2 == 2
                    then
                        if p3 == 1
                            then "Salmon, Goldfish, Guppy"
                        else if p3 == 2
                            then "Turtle, Crocodile, Snake"
                        else if p3 == 3
                            then "Frog, Toad, Newt"
                        else "Tidak Ada"
                    else "Tidak Ada"
        else if p1 == 2
            then
                if p2 == 1
                    then
                        if p3 == 1
                            then "Ant, Cockroach, Ladybug"
                        else if p3 == 2
                            then "Scorpion, Spider, Millipede"
                        else "Tidak Ada"
                    else if p2 == 2
                        then
                            if p3 == 1
                                then "Earthworm, Leech"
                            else if p3 == 2
                                then "Fluke Worm, Tapeworm"
                            else "Tidak Ada"
                        else "Tidak Ada"
            else "Tidak Ada"

main :: IO ()
main = do
    p1 <- readLn :: IO Int
    putStrLn (subfilum p1)
    p2 <- readLn :: IO Int
    putStrLn (ciri p1 p2)
    p3 <- readLn :: IO Int
    putStrLn (kelas p1 p2 p3)
    putStrLn (hewan p1 p2 p3)