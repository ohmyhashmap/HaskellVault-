data Person = Person { name :: String, age :: Int } deriving Show

createPerson :: String -> Int -> Person
createPerson n a = Person { name = n, age = a }

getAge :: Person -> Int
getAge person = age person

getName :: Person -> String
getName person = name person

main :: IO()
main = do
    let aku = createPerson "Christian" 12
    -- Menggunakan putStrLn untuk mencetak tanpa tanda kutip
    putStrLn (getName aku)
    print (getAge aku)