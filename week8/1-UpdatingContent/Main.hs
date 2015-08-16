import Network.HTTP.Conduit (simpleHttp)
import Text.HTML.TagSoup
import Data.ByteString.Lazy.Char8 (unpack)
import Data.Text (strip, pack)
import qualified Data.Text as T (unpack)
import Data.List (intercalate)
import System.Directory (doesFileExist)

isFinished :: [Tag String] -> Bool
isFinished [] = True
isFinished (TagOpen "em" [] : TagText "Note: this book is in progress" : xs) =
    False
isFinished (_ : xs) = isFinished xs

fetchToc :: [Tag String] -> String
fetchToc [] = error "Could not find a `div' element with class name `toc'!"
fetchToc (TagOpen "div" [("class", "toc")] : xs) = helper xs
    where
        helper :: [Tag String] -> String
        helper [] =
            error "Could not find a closing tag for the TOC `div' element!"
        helper (TagClose "div" : xs) = ""
        helper (TagOpen "li" [] : TagText entry : xs) =
            (T.unpack $ strip $ pack entry) ++ "\n" ++ helper xs
        helper (_ : xs) = helper xs
fetchToc (_ : xs) = fetchToc xs

newLines :: String -> String -> String
newLines oldstr newstr = intercalate "\n" $
    drop (length $ lines oldstr) (lines newstr)

main = do
    l <- fmap (parseTags . unpack) (simpleHttp =<< Prelude.getLine)
    if isFinished l
        then putStrLn "The book has already been completed.  Finally!"
        else do
            putStr "The book has not yet been completed"
            doesLastTocFileExist <- doesFileExist "last-toc"
            lastToc <- if doesLastTocFileExist
                then readFile "last-toc"
                else return ""
            let toc     = fetchToc l
                tocDiff = newLines lastToc toc in do
                if null tocDiff
                    then putStrLn ", and no new chapters have been added.  Try again later!"
                    else do
                        putStrLn $ ", but the following chapters have been added:\n" ++ tocDiff
                        writeFile "last-toc" toc
