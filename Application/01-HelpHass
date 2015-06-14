fastestRoute :: [(Char, Char)] -> [Char]
fastestRoute [] = []
fastestRoute routes = shortestPathBFS routes 'L' ['H'] ['H'] where
    shortestPathBFS graph end queue path
        | null queue    =  []
        | elem end adjacencies  =  path ++ [end]
        | otherwise     =  head (filter (\next -> not (null next)) (map reccall adjacencies))
        where
            vertex         =  head queue
            adjacencies    =  adjacentVertices graph vertex
            newQueue next  =  if elem next path then tail queue else (tail queue) ++ [next]
            newPath  next  =  if elem next path then path       else path ++ [next]
            reccall  next  =  shortestPathBFS graph end (newQueue next) (newPath next)


adjacentVertices :: Eq a => [(a, a)] -> a -> [a]
adjacentVertices [] vertex = []
adjacentVertices (edge : edges) vertex
    | fst edge == vertex  =  snd edge : adjacentVertices edges vertex
    | otherwise           =  adjacentVertices edges vertex
