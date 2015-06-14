split :: Eq a => [a] -> [a] -> [[a]]
split [] string     =  [string]
split delimiter []  =  []
split delimiter string
    | position == -1    =  [string]
    | otherwise         =  take position string : split delimiter (drop (position + (length delimiter)) string)
    where
        position = find delimiter string


find :: Eq a => [a] -> [a] -> Int
find needle []               =  -1
find needle haystack
    | match needle haystack  =  0
    | otherwise              =  1 + find needle (tail haystack)


match :: Eq a => [a] -> [a] -> Bool
match []     haystack = True
match needle []       = False
match needle haystack
    | head needle /= head haystack  =  False
    | otherwise                     =  match (tail needle) (tail haystack)
