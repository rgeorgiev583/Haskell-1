split :: Eq a => [a] -> [a] -> [[a]]
split [] string     =  [string]
split delimiter []  =  []
split delimiter string
    | position == -1    =  [string]
    | otherwise         =  take position string : split delimiter (drop (position + (length delimiter)) string)
    where
        position = find delimiter string


find :: Eq a => [a] -> [a] -> Int
find needle haystack = helper needle haystack 0 where
    helper needle [] position    =  -1
    helper needle haystack position
        | match needle haystack  =  position
        | otherwise              =  helper needle (tail haystack) (position + 1)


match :: Eq a => [a] -> [a] -> Bool
match []     haystack = True
match needle []       = False
match needle haystack
    | head needle /= head haystack  =  False
    | otherwise                     =  match (tail needle) (tail haystack)


reverseMatch :: Eq a => [a] -> [a] -> Bool
reverseMatch []     haystack = True
reverseMatch needle []       = False
reverseMatch needle haystack
    | last needle /= last haystack  =  False
    | otherwise                     =  match (init needle) (init haystack)
