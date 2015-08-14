import Network.HTTP.Conduit (simpleHttp)
import Data.ByteString.Lazy.Char8 as L
import Text.HTML.TagSoup

main = show (fmap parseTags $ simpleHttp =<< getLine)
