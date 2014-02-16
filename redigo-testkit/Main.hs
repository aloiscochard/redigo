import Control.Monad
import Control.Monad.Primitive
import Control.Monad.Loops
import Control.Monad.Trans
import Data.ByteString (ByteString)
import Database.Redis
import System.Random.MWC

import qualified Data.ByteString as ByteString
import qualified Data.ByteString.Char8 as Char8

numberOfURL           = 2 * 1000 * 1000
numberOfURLPerURN     = 10
numberOfURN           = div numberOfURL numberOfURLPerURN

main :: IO ()
main = withSystemRandom $ \gen -> do
  conn  <- connect $ defaultConnectInfo { connectPort = redisSocket }
  _     <- generate conn gen
  return () where
    redisSocket       = UnixSocket "/tmp/redis.sock"

generate :: Connection -> Gen (PrimState IO) -> IO ()
generate conn gen = do
  replicateM_ numberOfURN generate
  return () where
    generate :: IO ()
    generate = do
      urn       <- genString gen
      urls      <- replicateM numberOfURLPerURN $ genString gen
      let rows  =  (("urn-" ++ urn), "http://www.haskell.org") : map (\x -> ("url-/" ++ x, urn)) urls
      _         <- sequence $ map (add conn) rows
      return () where
        add conn (k, v) = runRedis conn $ do
          set (Char8.pack k) (Char8.pack v)
          return ()

{-- TODO Move to commons --}
genString :: Gen (PrimState IO) -> IO String
genString gen = do
    xs <- replicateM 4 $ uniformR (start, end) gen
    return $ map toEnum xs where
      start = fromEnum 'a'
      end = fromEnum 'z'
