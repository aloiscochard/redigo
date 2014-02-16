import Control.Concurrent
import Control.Applicative
import Control.Monad
import Control.Monad.Trans
import Data.ByteString (ByteString)
import Data.Function
import Database.Redis
import Network.CGI
import Network.FastCGI
import Network.URI

import qualified Data.ByteString as ByteString
import qualified Data.ByteString.Char8 as Char8

data Event = Redirect { request :: URI, response :: Response } | NotFound String deriving Show
data Response = Response { target :: String, urn :: String } deriving Show

cgiMain :: Connection -> CGI CGIResult
cgiMain conn = do
  uri       <- requestURI
  response  <- lift $ redisQuery conn uri
  _         <- lift . forkIO . redisUpdate conn $ maybe (NotFound $ show uri) (Redirect uri) response
  maybe (outputNotFound $ show uri) redirect $ fmap target response

redisQuery :: Connection -> URI -> IO (Maybe Response)
redisQuery conn uri = runRedis conn $ do
  urn         <- get urlKey
  let urnKey  = fmap (prefix "urn") (handleError urn)
  url         <- maybe liftMaybe get urnKey
  return $ on Response Char8.unpack <$> handleError url <*> urnKey where
    urlKey      = Char8.pack $ "url-" ++ uriPath uri
    liftMaybe = return . return $ Nothing

redisUpdate :: Connection -> Event -> IO ()
redisUpdate conn event = runRedis conn $ do
  handle event
  return () where
    counterOf = prefix "counter"
    handle (Redirect request response) = 
      incr $ counterOf (Char8.pack $ urn response)
    handle (NotFound urn) = do 
      lpush key [Char8.pack urn]
      incr $ counterOf key where
        key = Char8.pack "list-404"

prefix :: String -> ByteString -> ByteString
prefix p = ByteString.append (Char8.pack (p ++ "-"))

handleError :: Either b (Maybe a) -> Maybe a
handleError e = join $ right e where
  right = either (const Nothing) Just

main :: IO ()
main = do
  redisConn <- connect $ defaultConnectInfo { connectPort = redisSocket }
  runFastCGIConcurrent' forkIO maxCastCGIThreads (cgiMain redisConn) where 
    maxCastCGIThreads = 16
    redisSocket       = UnixSocket "/tmp/redis.sock"
