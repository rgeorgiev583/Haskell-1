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
    | rememberedMales && rememberedFemales                  =  100
    | rememberedMales || rememberedFemales                  =  50
    | otherwise                                             =  0
    where
        rememberedMales    =    maleNameCount == 0 && not hasMales   ||   maleNameCount > 0 && hasMales
        rememberedFemales  =  femaleNameCount == 0 && not hasFemales || femaleNameCount > 0 && hasFemales
        hasMales           =  hasElementThatReverseMatches "ss"  names
        hasFemales         =  hasElementThatReverseMatches "tta" names
