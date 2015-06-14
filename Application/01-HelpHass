fastestRoute :: [(a, a)] -> (a, a)
fastestRoute [] = []
fastestRoute routes = shortestPathBFS routes 'H' 'L' ['H'] ['H']
    where shortestPathBFS graph start end queue path
        | null queue    =  []
        | start == end  =  path
        | otherwise     =  head (filter (\next -> not null next) (map reccall (adjacentVertices graph vertex)))
            where
                vertex         =  head queue,
                newQueue next  =  if elem next path then tail queue else (tail queue) ++ [next],
                newPath  next  =  if elem next path then path       else path ++ [next],
                reccall  next  =  shortestPathBFS graph next end (newQueue next) (newPath next)


adjacentVertices :: [(a, a)] -> a -> [a]
adjacentVertices [] vertex = []
adjacentVertices (edge : edges) vertex
    | fst edge == vertex  =  snd edge : adjacentVertices edges vertex
    | otherwise           =  adjacentVertices edges vertex
