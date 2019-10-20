module Main where

import           Data.Function                  ( (&) )
import           Data.Maybe                     ( fromMaybe )
import           Lib                            ( app )
import           Network.Socket                 ( HostName
                                                , PortNumber
                                                )
import           Network.Wai.Handler.Warp       ( run )
import           System.Environment             ( lookupEnv )
import           Text.Read                      ( readMaybe )
import qualified Database.Redis                as Redis

main :: IO ()
main = do
  redisHostName <- getRedisHostName
  redisPort     <- getRedisPort
  let redisConnectInfo = Redis.defaultConnectInfo
        { Redis.connectHost = redisHostName
        , Redis.connectPort = redisPort
        }
  conn <- Redis.checkedConnect redisConnectInfo
  run 8081 (app conn)

getRedisHostName :: IO HostName
getRedisHostName = fromMaybe "localhost" <$> lookupEnv "REDIS_HOSTNAME"

getRedisPort :: IO Redis.PortID
getRedisPort = fromMaybe (Redis.PortNumber 6379) <$> portFromEnv
 where
  portFromEnv = do
    portFromEnvMaybe <- lookupEnv "REDIS_PORT"
    portFromEnvMaybe 
      >>= readMaybe @PortNumber
      & fmap Redis.PortNumber
      & pure
