import Network.HTTP.Conduit (simpleHttp)
import Text.HTML.TagSoup
import Data.ByteString.Lazy.Char8 (unpack)

isFinished :: [Tag String] -> IO ()
isFinished [] = putStrLn "Yes."
isFinished (TagOpen "em" [] : TagText "Note: this book is in progress" : xs) = putStrLn "No."
isFinished (_ : xs) = isFinished xs

main = isFinished =<< (fmap (\bs -> parseTags $ unpack bs) $ simpleHttp =<< Prelude.getLine)
