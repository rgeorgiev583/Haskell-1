reverseMatch []     haystack        =  True
reverseMatch needle []              =  False
reverseMatch needle haystack
    | last needle /= last haystack  =  False
    | otherwise                     =  reverseMatch (init needle) (init haystack)


hasElementThatReverseMatches needle []    =  False
hasElementThatReverseMatches needle list
    | reverseMatch needle (head list)     =  True
    | otherwise                           =  hasElementThatReverseMatches needle (tail list)


rememberedNameQuotient maleNameCount femaleNameCount []     =  100
rememberedNameQuotient maleNameCount femaleNameCount names
    | rememberedMales && rememberedFemales  =  100
    | rememberedMales || rememberedFemales  =  50
    | otherwise                             =  0
    where
        rememberedMales    =    maleNameCount > 0 && hasElementThatReverseMatches "ss"  names
        rememberedFemales  =  femaleNameCount > 0 && hasElementThatReverseMatches "tta" names
