import Network.HTTP.Conduit (simpleHttp)
import Text.HTML.TagSoup
import Data.ByteString.Lazy.Char8 (unpack)

isFinished :: [Tag String] -> Bool
isFinished [] = True
isFinished (TagOpen "em" [] : TagText "Note: this book is in progress" : xs) = False
isFinished (_ : xs) = isFinished xs

fetchToc :: [Tag String] -> String
fetchToc [] = error "Could not find a `div' element with class name `toc'!"
fetchToc (TagOpen "div" [("class", "toc")] : xs) = helper xs
    where
        helper :: [Tag String] -> String
        helper [] = error "Could not find a closing tag for the TOC `div' element!"
        helper (TagClose "div" : xs) = ""
        helper (TagOpen "li" [] : TagText entry : xs) = entry ++ "\n" ++ helper xs
        helper (_ : xs) = helper xs
fetchToc (_ : xs) = fetchToc xs

main = fmap (parseTags . unpack) (simpleHttp =<< Prelude.getLine) >>=
    \l -> if   isFinished l
          then putStrLn "Yes."
          else putStrLn "No."
