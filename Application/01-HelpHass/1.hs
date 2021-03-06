-- Usage:  Load the source file in GHCi and enter the following input in the REPL to run the example tests:
-- *Main> fastestRoute [('H', 'F'), ('F', 'L'), ('H', 'L')]
-- *Main> fastestRoute [('H', 'A'), ('K', 'L'), ('S', 'K'), ('A', 'S')]


fastestRoute routes = shortestPathBFS routes 'L' ['H'] ['H']


shortestPathBFS graph end queue path
    | null queue              =  []
    | end `elem` adjacencies  =  path ++ [end]
    | null list               =  []
    | otherwise               =  head list
    where
        vertex         =  head queue
        adjacencies    =  adjacentVertices graph vertex
        newQueue next  =  if next `elem` path then tail queue else tail queue   ++ [next]
        newPath  next  =  if next `elem` path then path       else path         ++ [next]
        reccall  next  =  shortestPathBFS graph end (newQueue next) (newPath next)
        list           =  filter (\next -> not (null next)) (map reccall adjacencies)


adjacentVertices [] vertex = []
adjacentVertices (edge : edges) vertex
    | fst edge == vertex   =  snd edge : adjacentVertices edges vertex
    | otherwise            =             adjacentVertices edges vertex
