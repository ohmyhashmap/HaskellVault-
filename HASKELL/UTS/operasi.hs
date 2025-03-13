fungsi1 :: Int -> Int
fungsi1 x = x + 5 

fungsi2 :: Int -> Int
fungsi2 x = x - 5 

fungsi3 :: Int -> Int
fungsi3 x = x 'mod' 5 

fungsi4 :: Int -> Int
fungsi4 x = x 'div' 5 

fungsi5 :: Int -> Int
fungsi5 x = x * 5 

fungsi6 :: Int -> Int
fungsi6 x = x + 5 

fungsi7 :: Int -> Int
fungsi7 x = x + 5 

fungsi8 :: Int -> Int -> Bool
fungsi8 x y = x == y

fungsi9 :: Int -> Int -> Bool
fungsi9 x y = x /= y

main = do
    print(fungsi1 3)
    print(fungsi2 3)
    print(fungsi3 3)
    print(fungsi4 3)
    print(fungsi5 3)
    print(fungsi6 3)    
    print(fungsi7 3)