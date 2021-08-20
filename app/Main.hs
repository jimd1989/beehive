module Main where

import Protolude (Int, IO, (.), ($), (==), pure, readMaybe)
import Control.Monad (guard)
import Data.ByteString.Lazy (ByteString)
import Data.ByteString.Lazy.Char8 (fromChunks)
import Data.HashMap.Strict (HashMap)
import Data.List ((!!), length)
import Data.Maybe (Maybe(..), fromMaybe)
import Data.Text (Text, pack)
import Data.Text.Encoding (encodeUtf8)
import Data.Tuple (fst, snd)
import GHC.IO (FilePath)
import Network.Wai (Response, Request, pathInfo, responseLBS)
import Network.Wai.Handler.Warp (run)
import Network.HTTP.Types (Status, status200, status400, status404)
import Network.HTTP.Types.Header (hContentType)
import System.Environment (getArgs)
import Hash (Hash, dictionary)
import Helpers ((⊙))
import Query (query)

main ∷ IO ()
main = do
  args ← parseArgs
  port ← pure $ fst args
  file ← pure $ snd args
  dict ← dictionary file
  run port (\req send → send $ route dict req)

parseArgs ∷ IO (Int, FilePath)
parseArgs = do
  args ← getArgs
  guard ((length args) == 2)
  port ← pure $ fromMaybe 3000 (readMaybe $ args !! 0)
  file ← pure $ args !! 1
  pure $ (port, file)

route ∷ HashMap Hash ByteString → Request → Response
route dict α = case pathInfo α of
 ("answer" : ω : []) → handleQuery dict ω
 ([]               ) → textResponse status200 homeMsg
 (_                ) → textResponse status404 "invalid path"

handleQuery ∷ HashMap Hash ByteString → Text → Response
handleQuery dict α = case query dict α of
  (Just ω ) → okResponse ω
  (Nothing) → textResponse status400 "malformated puzzle query"

okResponse ∷ ByteString → Response
okResponse α = responseLBS status200 [(hContentType, "text/plain")] α

textResponse ∷ Status → Text → Response
textResponse α ω = responseLBS α [(hContentType, "text/plain")] (convert ω)
  where convert = fromChunks . pure . encodeUtf8

homeMsg ∷ Text
homeMsg = "Request answers with `/answer/abcdefg`, where `a` \
           \is the essential letter"
