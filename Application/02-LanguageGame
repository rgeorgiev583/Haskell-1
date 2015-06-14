isInLanguage :: [Char] -> [Char] -> Bool
isInLanguage language word  =  validate rules word 0 (getFirstLength word (head word))
    where
        rules = map (\rule -> head rule) (wordsWhen (== "^n") language)


validate []    [] length expectedLength  =  True
validate [] word  length expectedLength  =  False
validate rules [] length expectedLength  =  False
validate rules word length expectedLength
    | length < expectedLength  =
        if head word /= head rules then
            False
        else
            validate rules (tail word) (length + 1) expectedLength
    | otherwise  =  validate (tail rules) word 0 expectedLength


getFirstLength []    letter  =  0
getFirstLength word  letter
    | (head word) /= letter  =  0
    | otherwise              =  1 + getFirstLength (tail word) letter


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


wordsWhen :: (Char -> Bool) -> String -> [String]
wordsWhen p s  =  case dropWhile p s of
                       "" -> []
                       s'  -> w : wordsWhen p s''
                           where (w, s'') = break p s'
