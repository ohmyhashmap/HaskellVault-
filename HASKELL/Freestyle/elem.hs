main = do
    -- Check if 3 is in the list [1, 2, 3, 4]
    print (elem 3 [1, 2, 3, 4])    -- Output: True

    -- Check if "apple" is in the list ["banana", "orange", "apple"]
    print (elem "apple" ["banana", "orange", "apple"])    -- Output: True

    -- Check if 5 is in the list [1, 2, 3, 4]
    print (elem 5 [1, 2, 3, 4])    -- Output: False

    -- Using infix notation with elem
    print (3 `elem` [1, 2, 3, 4])    -- Output: True

    -- Check if 5 is not in the list [1, 2, 3, 4]
    print (5 `notElem` [1, 2, 3, 4])    -- Output: True
    
    -- Prefix form
    elem 3 [1, 2, 3, 4]  -- Reads as "elem of 3 in [1,2,3,4]"

    -- Infix form
    3 `elem` [1, 2, 3, 4]  -- Reads more naturally as "3 is an element of [1,2,3,4]"