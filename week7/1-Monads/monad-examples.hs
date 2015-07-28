main2 = do
  name <- getLine
  family <- getLine
  putStrLn $ name ++ " " ++ family

main2' = getLine >>= \name ->
    getLine >>= \family ->
      putStrLn $ name ++ " " ++ family

main3 = do
  putStr "hello"
  putStrLn " world"

main3' = putStr "hello" >>
    putStrLn " world"
